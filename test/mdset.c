/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Programmer:  Neil Fortner
 *              March 10, 2014
 *
 * Purpose:     Test H5Dwrite_multi() and H5Dread_multi using randomized
 *              parameters.  Also tests H5Dwrite() and H5Dread() using a similar
 *              method.
 */

#include "h5test.h"

#define NAME_BUF_SIZE   1024
#define MAX_DSETS 5
#define MAX_DSET_X 10
#define MAX_DSET_Y 10
#define MAX_CHUNK_X 4
#define MAX_CHUNK_Y 4
#define MAX_HS_X 6
#define MAX_HS_Y 6
#define MAX_HS 3
#define MAX_POINTS 6
#define OPS_PER_FILE 100
#define DSET_MAX_NAME_LEN 8

/* Option flags */
#define MDSET_FLAG_CHUNK        0x01u
#define MDSET_FLAG_SHAPESAME    0x02u
#define MDSET_FLAG_MDSET        0x04u
#define MDSET_ALL_FLAGS (MDSET_FLAG_CHUNK | MDSET_FLAG_SHAPESAME \
    | MDSET_FLAG_MDSET)

const char *FILENAME[] = {
    "mdset",
    NULL
};

/* Names for datasets */
char dset_name[MAX_DSETS][DSET_MAX_NAME_LEN];


/*-------------------------------------------------------------------------
 * Function:    test_mdset
 *
 * Purpose:     Test randomized I/O using one or more datasets.  Creates a
 *              file, runs OPS_PER_FILE read or write operations verifying
 *              that reads return the expected data, then closes the file.
 *              Runs the test with a new file niter times.
 *
 *              The operations can use either hyperslab or point
 *              selections.  Options are available for chunked or
 *              contiguous layout, use of multiple datasets and H5D*_multi
 *              calls, and use of the "shapesame" algorithm code path.  To
 *              avoid the shapesame path when that option is not set, this
 *              function simply adds a dimension to the memory buffer in a
 *              way that the shapesame code is not designed to handle.
 *
 * Return:      Number of errors
 *
 * Programmer:  Neil Fortner
 *              Monday, March 10, 2014
 *
 *-------------------------------------------------------------------------
 */
static int
test_mdset(size_t niter, unsigned flags, hid_t fapl_id)
{
    H5D_rw_multi_t multi_info[MAX_DSETS];
    size_t max_dsets;
    size_t buf_size;
    size_t ndsets;
    hid_t file_id;
    hid_t dcpl_id;
    hsize_t dset_dims[MAX_DSETS][3];
    hsize_t chunk_dims[2];
    hsize_t max_dims[2] = {H5S_UNLIMITED, H5S_UNLIMITED};
    unsigned *rbuf = NULL;
    unsigned *rbufi[MAX_DSETS][MAX_DSET_X];
    unsigned *erbuf = NULL;
    unsigned *erbufi[MAX_DSETS][MAX_DSET_X];
    unsigned *wbuf = NULL;
    unsigned *wbufi[MAX_DSETS][MAX_DSET_X];
    unsigned *efbuf = NULL;
    unsigned *efbufi[MAX_DSETS][MAX_DSET_X];
    hbool_t do_read;
    hsize_t start[3];
    hsize_t count[3];
    hsize_t points[3 * MAX_POINTS];
    char    filename[NAME_BUF_SIZE];
    size_t i, j, k, l, m, n;

    TESTING("random I/O");

    h5_fixname(FILENAME[0], fapl_id, filename, sizeof filename);

    /* Calculate maximum number of datasets */
    max_dsets = (flags & MDSET_FLAG_MDSET) ? MAX_DSETS : 1;

    /* Calculate buffer size */
    buf_size = max_dsets * MAX_DSET_X * MAX_DSET_Y * sizeof(unsigned);

    /* Allocate buffers */
    if(NULL == (rbuf = (unsigned *)HDmalloc(buf_size)))
        TEST_ERROR
    if(NULL == (erbuf = (unsigned *)HDmalloc(buf_size)))
        TEST_ERROR
    if(NULL == (wbuf = (unsigned *)HDmalloc(buf_size)))
        TEST_ERROR
    if(NULL == (efbuf = (unsigned *)HDmalloc(buf_size)))
        TEST_ERROR

    /* Initialize buffer indices */
    for(i = 0; i < max_dsets; i++)
        for(j = 0; j < MAX_DSET_X; j++) {
            rbufi[i][j] = rbuf + (i * MAX_DSET_X * MAX_DSET_Y) + (j * MAX_DSET_Y);
            erbufi[i][j] = erbuf + (i * MAX_DSET_X * MAX_DSET_Y) + (j * MAX_DSET_Y);
            wbufi[i][j] = wbuf + (i * MAX_DSET_X * MAX_DSET_Y) + (j * MAX_DSET_Y);
            efbufi[i][j] = efbuf + (i * MAX_DSET_X * MAX_DSET_Y) + (j * MAX_DSET_Y);
        } /* end for */

    /* Initialize 3rd dimension information (for tricking library into using
     * non-"shapesame" code */
    for(i = 0; i < max_dsets; i++)
        dset_dims[i][2] = 1;
    start[2] = 0;
    count[2] = 1;

    /* Initialize memory type */
    for(i = 0; i < max_dsets; i++)
        multi_info[i].mem_type_id = H5T_NATIVE_UINT;

    /* Generate memory dataspace */
    dset_dims[0][0] = MAX_DSET_X;
    dset_dims[0][1] = MAX_DSET_Y;
    if((multi_info[0].mem_space_id = H5Screate_simple((flags & MDSET_FLAG_SHAPESAME) ? 2 : 3, dset_dims[0], NULL)) < 0)
        TEST_ERROR
    for(i = 1; i < max_dsets; i++)
        if((multi_info[i].mem_space_id = H5Scopy(multi_info[0].mem_space_id)) < 0)
            TEST_ERROR

    /* Create dcpl */
    if((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR

    /* Set fill time to alloc, and alloc time to early (so we always know
     * what's in the file) */
    if(H5Pset_fill_time(dcpl_id, H5D_FILL_TIME_ALLOC) < 0)
        TEST_ERROR
    if(H5Pset_alloc_time(dcpl_id, H5D_ALLOC_TIME_EARLY) < 0)
        TEST_ERROR

    for(i = 0; i < niter; i++) {
        /* Determine number of datasets */
        ndsets = (flags & MDSET_FLAG_MDSET)
            ? (size_t)((size_t)HDrandom() % max_dsets) + 1 : 1;

        /* Create file */
        if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id)) < 0)
            TEST_ERROR

        /* Create datasets */
        for(j = 0; j < ndsets; j++) {
            /* Generate file dataspace */
            dset_dims[j][0] = (hsize_t)((HDrandom() % MAX_DSET_X) + 1);
            dset_dims[j][1] = (hsize_t)((HDrandom() % MAX_DSET_Y) + 1);
            if((multi_info[j].dset_space_id = H5Screate_simple(2, dset_dims[j], (flags & MDSET_FLAG_CHUNK) ? max_dims : NULL)) < 0)
                TEST_ERROR

            /* Generate chunk (if requested) */
            if(flags & MDSET_FLAG_CHUNK) {
                chunk_dims[0] = (hsize_t)((HDrandom() % MAX_CHUNK_X) + 1);
                chunk_dims[1] = (hsize_t)((HDrandom() % MAX_CHUNK_Y) + 1);
                if(H5Pset_chunk(dcpl_id, 2, chunk_dims) < 0)
                    TEST_ERROR
            } /* end if */

            /* Create dataset */
            if((multi_info[j].dset_id = H5Dcreate2(file_id, dset_name[j], H5T_NATIVE_UINT, multi_info[j].dset_space_id, H5P_DEFAULT, dcpl_id, H5P_DEFAULT)) < 0)
                TEST_ERROR
        } /* end for */

        /* Initialize read buffer and expected read buffer */
        (void)HDmemset(rbuf, 0, buf_size);
        (void)HDmemset(erbuf, 0, buf_size);

        /* Initialize write buffer */
        for(j = 0; j < max_dsets; j++)
            for(k = 0; k < MAX_DSET_X; k++)
                for(l = 0; l < MAX_DSET_Y; l++)
                    wbufi[j][k][l] = (unsigned)((j * MAX_DSET_X * MAX_DSET_Y) + (k * MAX_DSET_Y) + l);

        /* Initialize expected file buffer */
        (void)HDmemset(efbuf, 0, buf_size);

        /* Perform read/write operations */
        for(j = 0; j < OPS_PER_FILE; j++) {
            /* Decide whether to read or write */
            do_read = (hbool_t)(HDrandom() % 2);

            /* Loop over datasets */
            for(k = 0; k < ndsets; k++) {
                /* Reset selection */
                if(H5Sselect_none(multi_info[k].mem_space_id) < 0)
                    TEST_ERROR
                if(H5Sselect_none(multi_info[k].dset_space_id) < 0)
                    TEST_ERROR

                /* Decide whether to do a hyperslab or point selection */
                if(HDrandom() % 2) {
                    /* Hyperslab */
                    size_t nhs = (size_t)((HDrandom() % MAX_HS) + 1); /* Number of hyperslabs */
                    size_t max_hs_x = (MAX_HS_X <= dset_dims[k][0]) ? MAX_HS_X : dset_dims[k][0]; /* Determine maximum hyperslab size in X */
                    size_t max_hs_y = (MAX_HS_Y <= dset_dims[k][1]) ? MAX_HS_Y : dset_dims[k][1]; /* Determine maximum hyperslab size in Y */

                    for(l = 0; l < nhs; l++) {
                        /* Generate hyperslab */
                        count[0] = (hsize_t)(((hsize_t)HDrandom() % max_hs_x) + 1);
                        count[1] = (hsize_t)(((hsize_t)HDrandom() % max_hs_y) + 1);
                        start[0] = (count[0] == dset_dims[k][0]) ? 0
                                : (hsize_t)HDrandom() % (dset_dims[k][0] - count[0] + 1);
                        start[1] = (count[1] == dset_dims[k][1]) ? 0
                                : (hsize_t)HDrandom() % (dset_dims[k][1] - count[1] + 1);

                        /* Select hyperslab */
                        if(H5Sselect_hyperslab(multi_info[k].mem_space_id, H5S_SELECT_OR, start, NULL, count, NULL) < 0)
                            TEST_ERROR
                        if(H5Sselect_hyperslab(multi_info[k].dset_space_id, H5S_SELECT_OR, start, NULL, count, NULL) < 0)
                            TEST_ERROR

                        /* Update expected buffers */
                        if(do_read) {
                            for(m = start[0]; m < (start[0] + count[0]); m++)
                                for(n = start[1]; n < (start[1] + count[1]); n++)
                                    erbufi[k][m][n] = efbufi[k][m][n];
                        } /* end if */
                        else
                            for(m = start[0]; m < (start[0] + count[0]); m++)
                                for(n = start[1]; n < (start[1] + count[1]); n++)
                                    efbufi[k][m][n] = wbufi[k][m][n];
                    } /* end for */
                } /* end if */
                else {
                    /* Point selection */
                    size_t npoints = (size_t)(((size_t)HDrandom() % MAX_POINTS) + 1); /* Number of points */

                    /* Generate points, 2D if using "shapesame", 3D otherwise */
                    if(flags & MDSET_FLAG_SHAPESAME)
                        for(l = 0; l < npoints; l++) {
                            points[2 * l] = (unsigned)((hsize_t)HDrandom() % dset_dims[k][0]);
                            points[(2 * l) + 1] = (unsigned)((hsize_t)HDrandom() % dset_dims[k][1]);
                        } /* end for */
                    else
                        for(l = 0; l < npoints; l++) {
                            points[3 * l] = (unsigned)((hsize_t)HDrandom() % dset_dims[k][0]);
                            points[(3 * l) + 1] = (unsigned)((hsize_t)HDrandom() % dset_dims[k][1]);
                            points[(3 * l) + 2] = 0;
                        } /* end for */

                    /* Select points in memory */
                    if(H5Sselect_elements(multi_info[k].mem_space_id, H5S_SELECT_APPEND, npoints, points) < 0)
                        TEST_ERROR

                    /* Convert to 2D for file selection, if not using "shapesame" */
                    if(!(flags & MDSET_FLAG_SHAPESAME) && (npoints > 1))
                        for(l = 1; l < npoints; l++) {
                            points[2 * l] = points[3 * l];
                            points[(2 * l) + 1] = points[(3 * l) + 1];
                        } /* end for */

                    /* Select points in file */
                    if(H5Sselect_elements(multi_info[k].dset_space_id, H5S_SELECT_APPEND, npoints, points) < 0)
                        TEST_ERROR

                    /* Update expected buffers */
                    if(do_read) {
                        for(l = 0; l < npoints; l++)
                            erbufi[k][points[2 * l]][points[(2 * l) + 1]] = efbufi[k][points[2 * l]][points[(2 * l) + 1]];
                    } /* end if */
                    else
                        for(l = 0; l < npoints; l++)
                            efbufi[k][points[2 * l]][points[(2 * l) + 1]] = wbufi[k][points[2 * l]][points[(2 * l) + 1]];
                } /* end else */
            } /* end for */

            /* Perform I/O */
            if(do_read) {
                if(flags & MDSET_FLAG_MDSET) {
                    /* Set buffers */
                    for(k = 0; k < ndsets; k++)
                        multi_info[k].u.rbuf = rbufi[k][0];

                    /* Read datasets */
                    if(H5Dread_multi(file_id, H5P_DEFAULT, ndsets, multi_info) < 0)
                        TEST_ERROR
                } /* end if */
                else
                    /* Read */
                    if(H5Dread(multi_info[0].dset_id, multi_info[0].mem_type_id, multi_info[0].mem_space_id, multi_info[0].dset_space_id, H5P_DEFAULT, rbuf) < 0)
                        TEST_ERROR

                /* Verify data */
                if(0 != memcmp(rbuf, erbuf, buf_size))
                    TEST_ERROR
            } /* end if */
            else {
                if(flags & MDSET_FLAG_MDSET) {
                    /* Set buffers */
                    for(k = 0; k < ndsets; k++)
                        multi_info[k].u.wbuf = wbufi[k][0];

                    /* Write datasets */
                    if(H5Dwrite_multi(file_id, H5P_DEFAULT, ndsets, multi_info) < 0)
                        TEST_ERROR
                } /* end if */
                else
                    /* Write */
                    if(H5Dwrite(multi_info[0].dset_id, multi_info[0].mem_type_id, multi_info[0].mem_space_id, multi_info[0].dset_space_id, H5P_DEFAULT, wbuf) < 0)
                        TEST_ERROR

                /* Update wbuf */
                for(l = 0; l < max_dsets; l++)
                    for(m = 0; m < MAX_DSET_X; m++)
                        for(n = 0; n < MAX_DSET_Y; n++)
                            wbufi[l][m][n] += (unsigned)max_dsets * MAX_DSET_X * MAX_DSET_Y;
            } /* end else */
        } /* end for */

        /* Close */
        for(j = 0; j < ndsets; j++) {
            if(H5Dclose(multi_info[j].dset_id) < 0)
                TEST_ERROR
            if(H5Sclose(multi_info[j].dset_space_id) < 0)
                TEST_ERROR
        } /* end for */
        if(H5Fclose(file_id) < 0)
            TEST_ERROR
    } /* end for */

    /* Close */
    for(i = 0; i < max_dsets; i++)
        if(H5Sclose(multi_info[i].mem_space_id) < 0)
            TEST_ERROR
    if(H5Pclose(dcpl_id) < 0)
        TEST_ERROR
    free(rbuf);
    rbuf = NULL;
    free(erbuf);
    erbuf = NULL;
    free(wbuf);
    wbuf = NULL;
    free(efbuf);
    efbuf = NULL;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY {
        for(i = 0; i < max_dsets; i++) {
            H5Dclose(multi_info[i].dset_id);
            H5Sclose(multi_info[i].mem_space_id);
            H5Sclose(multi_info[i].dset_space_id);
        } /* end for */
        H5Fclose(file_id);
        H5Pclose(dcpl_id);
    } H5E_END_TRY
    if(rbuf)
        free(rbuf);
    if(erbuf)
        free(erbuf);
    if(wbuf)
        free(wbuf);
    if(efbuf)
        free(efbuf);

    return -1;
} /* end test_mdset() */


/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Runs all tests with all combinations of configuration
 *              flags.
 *
 * Return:      Success:        0
 *              Failue:         1
 *
 * Programmer:  Neil Fortner
 *              Monday, March 10, 2014
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t fapl_id;
    int nerrors = 0;
    unsigned i;
    int ret;

    h5_reset();
    fapl_id = h5_fileaccess();

    /* Initialize random number seed */
    HDsrandom((unsigned)HDtime(NULL));

    /* Fill dset_name array */
    for(i = 0; i < MAX_DSETS; i++) {
        if((ret = snprintf(dset_name[i], DSET_MAX_NAME_LEN, "dset%u", i)) < 0)
            TEST_ERROR
        if(ret >= DSET_MAX_NAME_LEN)
            TEST_ERROR
    } /* end for */

    for(i = 0; i <= MDSET_ALL_FLAGS; i++) {
        /* Print flag configuration */
        puts("\nConfiguration:");
        printf("  Layout:     %s\n", (i & MDSET_FLAG_CHUNK) ? "Chunked" : "Contiguous");
        printf("  Shape same: %s\n", (i & MDSET_FLAG_SHAPESAME) ? "Yes" : "No");
        printf("  I/O type:   %s\n", (i & MDSET_FLAG_MDSET) ? "Multi" : "Single");
    
        nerrors += test_mdset(100, i, fapl_id);
    }

    h5_cleanup(FILENAME, fapl_id);

    if(nerrors)
        goto error;
    puts("All multi dataset tests passed.");

    return 0;

error:
    nerrors = MAX(1, nerrors);
    printf("***** %d multi dataset TEST%s FAILED! *****\n",
            nerrors, 1 == nerrors ? "" : "S");
    return 1;
} /* end main() */

