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
 * Purpose:     Test H5Dwrite_multi() and H5Dread_multi using randomized
 *              parameters.  Also tests H5Dwrite() and H5Dread() using a similar
 *              method.
 */

#include "h5test.h"

#define NAME_BUF_SIZE     1024
#define MAX_DSETS         6
#define MAX_DSET_X        10
#define MAX_DSET_Y        10
#define MAX_CHUNK_X       4
#define MAX_CHUNK_Y       4
#define MAX_HS_X          6
#define MAX_HS_Y          6
#define MAX_HS            3
#define MAX_POINTS        6
#define OPS_PER_FILE      50
#define DSET_MAX_NAME_LEN 8
#define EXT_FILENAME      "mdset_ext.h5"
#define SOURCE_DS_NAME    "vds_source"

/* Option flags */
#define MDSET_FLAG_CHUNK     0x01u
#define MDSET_FLAG_MLAYOUT   0x02u
#define MDSET_FLAG_SHAPESAME 0x04u
#define MDSET_FLAG_MDSET     0x08u
#define MDSET_FLAG_TCONV     0x10u
#define MDSET_FLAG_FILTER    0x20u
#define MDSET_ALL_FLAGS                                                                                      \
    (MDSET_FLAG_CHUNK | MDSET_FLAG_MLAYOUT | MDSET_FLAG_SHAPESAME | MDSET_FLAG_MDSET | MDSET_FLAG_TCONV |    \
     MDSET_FLAG_FILTER)

static const char *FILENAME[] = {"mdset", "mdset1", "mdset2", NULL};

/* Names for datasets */
char dset_name[MAX_DSETS][DSET_MAX_NAME_LEN];

/* Whether these filters are available */
htri_t deflate_avail    = false;
htri_t fletcher32_avail = false;

static int
test_mdset_location(hid_t fapl_id)
{
    hid_t       file_id1, file_id2;
    herr_t      ret;
    hid_t       dset_ids[2];
    hid_t       mem_type_ids[2];
    hid_t       mem_space_ids[2];
    hid_t       file_space_ids[2];
    void       *rbufs[2];
    const void *wbufs[2];
    hsize_t     dset_dims[2];
    int        *buf = NULL;
    char        filename1[NAME_BUF_SIZE];
    char        filename2[NAME_BUF_SIZE];

    TESTING("mdset location");

    h5_fixname(FILENAME[1], fapl_id, filename1, sizeof filename1);
    h5_fixname(FILENAME[2], fapl_id, filename2, sizeof filename2);

    /* Create files */
    if ((file_id1 = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id)) < 0)
        TEST_ERROR;
    if ((file_id2 = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id)) < 0)
        TEST_ERROR;

    if (NULL == (buf = (int *)calloc(2 * MAX_DSET_X * MAX_DSET_Y, sizeof(int))))
        TEST_ERROR;

    /* Generate memory dataspace */
    dset_dims[0] = MAX_DSET_X;
    dset_dims[1] = MAX_DSET_Y;
    if ((file_space_ids[0] = H5Screate_simple(2, dset_dims, NULL)) < 0)
        TEST_ERROR;
    if ((file_space_ids[1] = H5Screate_simple(2, dset_dims, NULL)) < 0)
        TEST_ERROR;

    mem_space_ids[0] = H5S_ALL;
    mem_space_ids[1] = H5S_ALL;

    mem_type_ids[0] = H5T_NATIVE_UINT;
    mem_type_ids[1] = H5T_NATIVE_UINT;

    if ((dset_ids[0] = H5Dcreate2(file_id1, dset_name[0], H5T_NATIVE_UINT, file_space_ids[0], H5P_DEFAULT,
                                  H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if ((dset_ids[1] = H5Dcreate2(file_id2, dset_name[1], H5T_NATIVE_UINT, file_space_ids[1], H5P_DEFAULT,
                                  H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    wbufs[0] = buf;
    wbufs[1] = buf + (MAX_DSET_X * MAX_DSET_Y);

    H5E_BEGIN_TRY
    {
        ret = H5Dwrite_multi(2, dset_ids, mem_type_ids, mem_space_ids, file_space_ids, H5P_DEFAULT, wbufs);
    }
    H5E_END_TRY

    if (ret >= 0) {
        fprintf(stderr, "H5Dmulti_write with datasets in multiple files should fail.\n");
        TEST_ERROR;
    }

    rbufs[0] = buf;
    rbufs[1] = buf + (MAX_DSET_X * MAX_DSET_Y);

    H5E_BEGIN_TRY
    {
        ret = H5Dread_multi(2, dset_ids, mem_type_ids, mem_space_ids, file_space_ids, H5P_DEFAULT, rbufs);
    }
    H5E_END_TRY

    if (ret >= 0) {
        fprintf(stderr, "H5Dmulti_read with datasets in multiple files should fail.\n");
        TEST_ERROR;
    }

    H5Dclose(dset_ids[0]);
    H5Sclose(file_space_ids[0]);
    H5Dclose(dset_ids[1]);
    H5Sclose(file_space_ids[1]);
    H5Fclose(file_id1);
    H5Fclose(file_id2);

    if (buf)
        free(buf);

    PASSED();
    return 0;

error:
    if (buf)
        free(buf);
    return -1;
}

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
 *-------------------------------------------------------------------------
 */
static int
test_mdset(size_t niter, unsigned flags, hid_t fapl_id)
{
    hid_t       dset_ids[MAX_DSETS];
    hid_t       mem_type_ids[MAX_DSETS];
    hid_t       mem_space_ids[MAX_DSETS];
    hid_t       file_space_ids[MAX_DSETS];
    void       *rbufs[MAX_DSETS];
    const void *wbufs[MAX_DSETS];
    size_t      max_dsets;
    size_t      buf_size;
    size_t      ndsets;
    hid_t       file_id = H5I_INVALID_HID;
    hid_t       dcpl_id[MAX_DSETS];
    hsize_t     dset_dims[MAX_DSETS][3];
    hsize_t     chunk_dims[2];
    hsize_t     max_dims[2] = {H5S_UNLIMITED, H5S_UNLIMITED};
    unsigned   *rbuf        = NULL;
    unsigned   *rbufi[MAX_DSETS][MAX_DSET_X];
    unsigned   *erbuf = NULL;
    unsigned   *erbufi[MAX_DSETS][MAX_DSET_X];
    unsigned   *wbuf = NULL;
    unsigned   *wbufi[MAX_DSETS][MAX_DSET_X];
    unsigned   *efbuf = NULL;
    unsigned   *efbufi[MAX_DSETS][MAX_DSET_X];
    bool        do_read;
    hsize_t     start[3];
    hsize_t     count[3];
    hsize_t     points[3 * MAX_POINTS];
    char        filename[NAME_BUF_SIZE];
    size_t      i, j, k, l, m, n;

    TESTING("random I/O");

    h5_fixname(FILENAME[0], fapl_id, filename, sizeof filename);

    /* Calculate maximum number of datasets */
    max_dsets = (flags & MDSET_FLAG_MDSET) ? MAX_DSETS : 1;

    /* Calculate buffer size */
    buf_size = max_dsets * MAX_DSET_X * MAX_DSET_Y * sizeof(unsigned);

    /* Initialize dcpl_id array */
    for (i = 0; i < max_dsets; i++)
        dcpl_id[i] = -1;

    /* Allocate buffers */
    if (NULL == (rbuf = (unsigned *)malloc(buf_size)))
        TEST_ERROR;
    if (NULL == (erbuf = (unsigned *)malloc(buf_size)))
        TEST_ERROR;
    if (NULL == (wbuf = (unsigned *)malloc(buf_size)))
        TEST_ERROR;
    if (NULL == (efbuf = (unsigned *)malloc(buf_size)))
        TEST_ERROR;

    /* Initialize buffer indices */
    for (i = 0; i < max_dsets; i++)
        for (j = 0; j < MAX_DSET_X; j++) {
            rbufi[i][j]  = rbuf + (i * MAX_DSET_X * MAX_DSET_Y) + (j * MAX_DSET_Y);
            erbufi[i][j] = erbuf + (i * MAX_DSET_X * MAX_DSET_Y) + (j * MAX_DSET_Y);
            wbufi[i][j]  = wbuf + (i * MAX_DSET_X * MAX_DSET_Y) + (j * MAX_DSET_Y);
            efbufi[i][j] = efbuf + (i * MAX_DSET_X * MAX_DSET_Y) + (j * MAX_DSET_Y);
        } /* end for */

    /* Initialize 3rd dimension information (for tricking library into using
     * non-"shapesame" code */
    for (i = 0; i < max_dsets; i++)
        dset_dims[i][2] = 1;
    start[2] = 0;
    count[2] = 1;

    /* Initialize IDs */
    for (i = 0; i < max_dsets; i++) {
        dset_ids[i]       = -1;
        file_space_ids[i] = -1;
        mem_type_ids[i]   = H5T_NATIVE_UINT;
        mem_space_ids[i]  = -1;
    } /* end for */

    /* Generate memory dataspace */
    dset_dims[0][0] = MAX_DSET_X;
    dset_dims[0][1] = MAX_DSET_Y;
    if ((mem_space_ids[0] = H5Screate_simple((flags & MDSET_FLAG_SHAPESAME) ? 2 : 3, dset_dims[0], NULL)) < 0)
        TEST_ERROR;
    for (i = 1; i < max_dsets; i++)
        if ((mem_space_ids[i] = H5Scopy(mem_space_ids[0])) < 0)
            TEST_ERROR;

    /* Create dcpl 0 */
    if ((dcpl_id[0] = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    /* Set fill time to alloc, and alloc time to early (so we always know
     * what's in the file) */
    if (H5Pset_fill_time(dcpl_id[0], H5D_FILL_TIME_ALLOC) < 0)
        TEST_ERROR;
    if (H5Pset_alloc_time(dcpl_id[0], H5D_ALLOC_TIME_EARLY) < 0)
        TEST_ERROR;

    /* Set filters if requested */
    if (flags & MDSET_FLAG_FILTER) {
        if (fletcher32_avail)
            if (H5Pset_fletcher32(dcpl_id[0]) < 0)
                TEST_ERROR;
        if (deflate_avail)
            if (H5Pset_deflate(dcpl_id[0], 1) < 0)
                TEST_ERROR;
    }

    /* Copy dcpl 0 to other slots in dcpl_id array */
    for (i = 1; i < MAX_DSETS; i++)
        if ((dcpl_id[i] = H5Pcopy(dcpl_id[0])) < 0)
            TEST_ERROR;

    /* If this is a multi layout run, set up different filters and layouts. Chunked and virtual
     * datasets will be set every iteration (with different dims), and contiguous is the default, so
     * no need to set either of those. */
    if (flags & MDSET_FLAG_MLAYOUT) {
        /* Set filters on dataset 2 */
        if (fletcher32_avail)
            if (H5Pset_fletcher32(dcpl_id[2]) < 0)
                TEST_ERROR;
        if (deflate_avail)
            if (H5Pset_deflate(dcpl_id[2], 1) < 0)
                TEST_ERROR;

        /* Dataset 3 is compact */
        if (H5Pset_layout(dcpl_id[3], H5D_COMPACT) < 0)
            TEST_ERROR;

        /* Dataset 4 is external */
        if (H5Pset_external(dcpl_id[4], EXT_FILENAME, 0, H5F_UNLIMITED) < 0)
            TEST_ERROR;
    }

    for (i = 0; i < niter; i++) {
        /* Determine number of datasets */
        ndsets = (flags & MDSET_FLAG_MLAYOUT) ? 6
                 : (flags & MDSET_FLAG_MDSET) ? (size_t)((size_t)HDrandom() % max_dsets) + 1
                                              : 1;

        /* Create file */
        if ((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id)) < 0)
            TEST_ERROR;

        /* Create datasets */
        for (j = 0; j < ndsets; j++) {
            hid_t source_dset;

            bool use_chunk =
                (flags & MDSET_FLAG_CHUNK) || ((flags & MDSET_FLAG_MLAYOUT) && (j == 1 || j == 2));

            /* Generate file dataspace */
            dset_dims[j][0] = (hsize_t)((HDrandom() % MAX_DSET_X) + 1);
            dset_dims[j][1] = (hsize_t)((HDrandom() % MAX_DSET_Y) + 1);
            if ((file_space_ids[j] = H5Screate_simple(2, dset_dims[j], use_chunk ? max_dims : NULL)) < 0)
                TEST_ERROR;

            /* Generate chunk if called for by configuration (multi layout uses chunked for datasets
             * 1 and 2) */
            if (use_chunk) {
                chunk_dims[0] = (hsize_t)((HDrandom() % MAX_CHUNK_X) + 1);
                chunk_dims[1] = (hsize_t)((HDrandom() % MAX_CHUNK_Y) + 1);
                if (H5Pset_chunk(dcpl_id[j], 2, chunk_dims) < 0)
                    TEST_ERROR;
            } /* end if */
            else if ((flags & MDSET_FLAG_CHUNK) && j == 5) {
                /* Dataset 5 is virtual in multi layout case */
                /* Set to contiguous to clear previous VDS settings */
                if (H5Pset_layout(dcpl_id[j], H5D_CONTIGUOUS) < 0)
                    TEST_ERROR;

                /* Set virtual dataset layout, ALL<>ALL mapping */
                if (H5Pset_virtual(dcpl_id[j], file_space_ids[j], ".", SOURCE_DS_NAME, file_space_ids[j]) < 0)
                    TEST_ERROR;
            }

            /* Create dataset */
            /* If MDSET_FLAG_TCONV is set, use a different datatype with 50% probability, so
             * some datasets require type conversion and others do not */
            if ((dset_ids[j] = H5Dcreate2(file_id, dset_name[j],
                                          (flags & MDSET_FLAG_TCONV && HDrandom() % 2) ? H5T_NATIVE_LONG
                                                                                       : H5T_NATIVE_UINT,
                                          file_space_ids[j], H5P_DEFAULT, dcpl_id[j], H5P_DEFAULT)) < 0)
                TEST_ERROR;

            /* Create virtual source dataset if necessary.  Use dcpl_id[0] for a contiguous dataset
             */
            if ((flags & MDSET_FLAG_MLAYOUT) && (j == 6)) {
                if ((source_dset = H5Dcreate2(file_id, SOURCE_DS_NAME,
                                              (flags & MDSET_FLAG_TCONV && HDrandom() % 2) ? H5T_NATIVE_LONG
                                                                                           : H5T_NATIVE_UINT,
                                              file_space_ids[j], H5P_DEFAULT, dcpl_id[0], H5P_DEFAULT)) < 0)
                    TEST_ERROR;
                if (H5Dclose(source_dset) < 0)
                    TEST_ERROR;
            }
        } /* end for */

        /* Initialize read buffer and expected read buffer */
        (void)memset(rbuf, 0, buf_size);
        (void)memset(erbuf, 0, buf_size);

        /* Initialize write buffer */
        for (j = 0; j < max_dsets; j++)
            for (k = 0; k < MAX_DSET_X; k++)
                for (l = 0; l < MAX_DSET_Y; l++)
                    wbufi[j][k][l] = (unsigned)((j * MAX_DSET_X * MAX_DSET_Y) + (k * MAX_DSET_Y) + l);

        /* Initialize expected file buffer */
        (void)memset(efbuf, 0, buf_size);

        /* Perform read/write operations */
        for (j = 0; j < OPS_PER_FILE; j++) {
            /* Decide whether to read or write.  Can't read on the first iteration with external
             * layout because the write is needed to create the external file. */
            do_read = (j == 0 && flags & MDSET_FLAG_MLAYOUT) ? false : (bool)(HDrandom() % 2);

            /* Loop over datasets */
            for (k = 0; k < ndsets; k++) {
                int sel_type;

                /* Reset selection */
                if (H5Sselect_none(mem_space_ids[k]) < 0)
                    TEST_ERROR;
                if (H5Sselect_none(file_space_ids[k]) < 0)
                    TEST_ERROR;

                /* Decide whether to do a hyperslab, point, or all selection */
                sel_type = HDrandom() % 3;
                if (sel_type == 0) {
                    /* Hyperslab */
                    size_t nhs      = (size_t)((HDrandom() % MAX_HS) + 1); /* Number of hyperslabs */
                    size_t max_hs_x = (MAX_HS_X <= dset_dims[k][0])
                                          ? MAX_HS_X
                                          : dset_dims[k][0]; /* Determine maximum hyperslab size in X */
                    size_t max_hs_y = (MAX_HS_Y <= dset_dims[k][1])
                                          ? MAX_HS_Y
                                          : dset_dims[k][1]; /* Determine maximum hyperslab size in Y */

                    for (l = 0; l < nhs; l++) {
                        /* Generate hyperslab */
                        count[0] = (hsize_t)(((hsize_t)HDrandom() % max_hs_x) + 1);
                        count[1] = (hsize_t)(((hsize_t)HDrandom() % max_hs_y) + 1);
                        start[0] = (count[0] == dset_dims[k][0])
                                       ? 0
                                       : (hsize_t)HDrandom() % (dset_dims[k][0] - count[0] + 1);
                        start[1] = (count[1] == dset_dims[k][1])
                                       ? 0
                                       : (hsize_t)HDrandom() % (dset_dims[k][1] - count[1] + 1);

                        /* Select hyperslab */
                        if (H5Sselect_hyperslab(mem_space_ids[k], H5S_SELECT_OR, start, NULL, count, NULL) <
                            0)
                            TEST_ERROR;
                        if (H5Sselect_hyperslab(file_space_ids[k], H5S_SELECT_OR, start, NULL, count, NULL) <
                            0)
                            TEST_ERROR;

                        /* Update expected buffers */
                        if (do_read) {
                            for (m = start[0]; m < (start[0] + count[0]); m++)
                                for (n = start[1]; n < (start[1] + count[1]); n++)
                                    erbufi[k][m][n] = efbufi[k][m][n];
                        } /* end if */
                        else
                            for (m = start[0]; m < (start[0] + count[0]); m++)
                                for (n = start[1]; n < (start[1] + count[1]); n++)
                                    efbufi[k][m][n] = wbufi[k][m][n];
                    } /* end for */
                }     /* end if */
                else if (sel_type == 1) {
                    /* Point selection */
                    size_t npoints = (size_t)(((size_t)HDrandom() % MAX_POINTS) + 1); /* Number of points */

                    /* Generate points */
                    for (l = 0; l < npoints; l++) {
                        points[2 * l]       = (unsigned)((hsize_t)HDrandom() % dset_dims[k][0]);
                        points[(2 * l) + 1] = (unsigned)((hsize_t)HDrandom() % dset_dims[k][1]);
                    } /* end for */

                    /* Select points in file */
                    if (H5Sselect_elements(file_space_ids[k], H5S_SELECT_APPEND, npoints, points) < 0)
                        TEST_ERROR;

                    /* Update expected buffers */
                    if (do_read) {
                        for (l = 0; l < npoints; l++)
                            erbufi[k][points[2 * l]][points[(2 * l) + 1]] =
                                efbufi[k][points[2 * l]][points[(2 * l) + 1]];
                    } /* end if */
                    else
                        for (l = 0; l < npoints; l++)
                            efbufi[k][points[2 * l]][points[(2 * l) + 1]] =
                                wbufi[k][points[2 * l]][points[(2 * l) + 1]];

                    /* Convert to 3D for memory selection, if not using
                     * "shapesame" */
                    if (!(flags & MDSET_FLAG_SHAPESAME)) {
                        for (l = npoints - 1; l > 0; l--) {
                            points[(3 * l) + 2] = 0;
                            points[(3 * l) + 1] = points[(2 * l) + 1];
                            points[3 * l]       = points[2 * l];
                        } /* end for */
                        points[2] = 0;
                    } /* end if */

                    /* Select points in memory */
                    if (H5Sselect_elements(mem_space_ids[k], H5S_SELECT_APPEND, npoints, points) < 0)
                        TEST_ERROR;
                } /* end else */
                else {
                    /* All selection */
                    /* Select entire dataset in file */
                    if (H5Sselect_all(file_space_ids[k]) < 0)
                        TEST_ERROR;

                    /* Select entire dataset in memory using hyperslab */
                    start[0] = 0;
                    start[1] = 0;
                    count[0] = dset_dims[k][0];
                    count[1] = dset_dims[k][1];
                    if (H5Sselect_hyperslab(mem_space_ids[k], H5S_SELECT_SET, start, NULL, count, NULL) < 0)
                        TEST_ERROR;

                    /* Update expected buffers */
                    if (do_read) {
                        for (m = 0; m < dset_dims[k][0]; m++)
                            for (n = 0; n < dset_dims[k][1]; n++)
                                erbufi[k][m][n] = efbufi[k][m][n];
                    } /* end if */
                    else
                        for (m = 0; m < dset_dims[k][0]; m++)
                            for (n = 0; n < dset_dims[k][1]; n++)
                                efbufi[k][m][n] = wbufi[k][m][n];
                }
            } /* end for */

            /* Perform I/O */
            if (do_read) {
                if (flags & MDSET_FLAG_MDSET) {
                    /* Set buffers */
                    for (k = 0; k < ndsets; k++)
                        rbufs[k] = rbufi[k][0];

                    /* Read datasets */
                    if (H5Dread_multi(ndsets, dset_ids, mem_type_ids, mem_space_ids, file_space_ids,
                                      H5P_DEFAULT, rbufs) < 0)
                        TEST_ERROR;
                } /* end if */
                else
                    /* Read */
                    if (H5Dread(dset_ids[0], mem_type_ids[0], mem_space_ids[0], file_space_ids[0],
                                H5P_DEFAULT, rbuf) < 0)
                        TEST_ERROR;

                /* Verify data */
                if (0 != memcmp(rbuf, erbuf, buf_size))
                    TEST_ERROR;
            } /* end if */
            else {
                if (flags & MDSET_FLAG_MDSET) {
                    /* Set buffers */
                    for (k = 0; k < ndsets; k++)
                        wbufs[k] = wbufi[k][0];

                    /* Write datasets */
                    if (H5Dwrite_multi(ndsets, dset_ids, mem_type_ids, mem_space_ids, file_space_ids,
                                       H5P_DEFAULT, wbufs) < 0)
                        TEST_ERROR;
                } /* end if */
                else
                    /* Write */
                    if (H5Dwrite(dset_ids[0], mem_type_ids[0], mem_space_ids[0], file_space_ids[0],
                                 H5P_DEFAULT, wbuf) < 0)
                        TEST_ERROR;

                /* Update wbuf */
                for (l = 0; l < max_dsets; l++)
                    for (m = 0; m < MAX_DSET_X; m++)
                        for (n = 0; n < MAX_DSET_Y; n++)
                            wbufi[l][m][n] += (unsigned)max_dsets * MAX_DSET_X * MAX_DSET_Y;
            } /* end else */
        }     /* end for */

        /* Close */
        for (j = 0; j < ndsets; j++) {
            if (H5Dclose(dset_ids[j]) < 0)
                TEST_ERROR;
            dset_ids[j] = -1;
            if (H5Sclose(file_space_ids[j]) < 0)
                TEST_ERROR;
            file_space_ids[j] = -1;
        } /* end for */
        if (H5Fclose(file_id) < 0)
            TEST_ERROR;
        file_id = -1;

        /* Cleanup external file.  Need to do this because otherwise there is garbage when the
         * dataset is created, even with early allocation and fill time. */
        HDremove(EXT_FILENAME);
    } /* end for */

    /* Close */
    for (i = 0; i < max_dsets; i++) {
        if (H5Sclose(mem_space_ids[i]) < 0)
            TEST_ERROR;
        mem_space_ids[i] = -1;
    } /* end for */
    for (i = 0; i < MAX_DSETS; i++) {
        if (H5Pclose(dcpl_id[i]) < 0)
            TEST_ERROR;
        dcpl_id[i] = -1;
    }
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
    H5E_BEGIN_TRY
    {
        for (i = 0; i < max_dsets; i++) {
            H5Dclose(dset_ids[i]);
            H5Sclose(mem_space_ids[i]);
            H5Sclose(file_space_ids[i]);
            H5Pclose(dcpl_id[i]);
        } /* end for */
        H5Fclose(file_id);
    }
    H5E_END_TRY
    if (rbuf)
        free(rbuf);
    if (erbuf)
        free(erbuf);
    if (wbuf)
        free(wbuf);
    if (efbuf)
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
 *              Failure:        1
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t    fapl_id;
    int      nerrors = 0;
    unsigned i;
    int      ret;

    h5_reset();
    fapl_id = h5_fileaccess();

    /* Initialize random number seed */
    HDsrandom((unsigned)HDtime(NULL));

    /* Fill dset_name array */
    for (i = 0; i < MAX_DSETS; i++) {
        if ((ret = snprintf(dset_name[i], DSET_MAX_NAME_LEN, "dset%u", i)) < 0)
            TEST_ERROR;
        if (ret >= DSET_MAX_NAME_LEN)
            TEST_ERROR;
    } /* end for */

    /* Check if deflate and fletcher32 filters are available */
    if ((deflate_avail = H5Zfilter_avail(H5Z_FILTER_DEFLATE)) < 0)
        TEST_ERROR;
    if ((fletcher32_avail = H5Zfilter_avail(H5Z_FILTER_FLETCHER32)) < 0)
        TEST_ERROR;

    for (i = 0; i <= MDSET_ALL_FLAGS; i++) {
        /* Skip incompatible flag combinations */
        if (((i & MDSET_FLAG_MLAYOUT) && (i & MDSET_FLAG_CHUNK)) ||
            ((i & MDSET_FLAG_MLAYOUT) && !(i & MDSET_FLAG_MDSET)) ||
            ((i & MDSET_FLAG_FILTER) && !(i & MDSET_FLAG_CHUNK)))
            continue;

        /* Print flag configuration */
        puts("\nConfiguration:");
        printf("  Layout:          %s\n", (i & MDSET_FLAG_MLAYOUT) ? "Multi"
                                          : (i & MDSET_FLAG_CHUNK) ? "Chunked"
                                                                   : "Contiguous");
        printf("  Shape same:      %s\n", (i & MDSET_FLAG_SHAPESAME) ? "Yes" : "No");
        printf("  I/O type:        %s\n", (i & MDSET_FLAG_MDSET) ? "Multi" : "Single");
        printf("  Type conversion: %s\n", (i & MDSET_FLAG_TCONV) ? "Yes" : "No");
        printf("  Data filter:     %s\n", (i & MDSET_FLAG_MLAYOUT)  ? "Mixed"
                                          : (i & MDSET_FLAG_FILTER) ? "Yes"
                                                                    : "No");

        nerrors += test_mdset(50, i, fapl_id);
    }

    /* test all datasets in same container */
    nerrors += test_mdset_location(fapl_id);

    h5_cleanup(FILENAME, fapl_id);

    if (nerrors)
        goto error;
    puts("All multi dataset tests passed.");

    return 0;

error:
    nerrors = MAX(1, nerrors);
    printf("***** %d multi dataset TEST%s FAILED! *****\n", nerrors, 1 == nerrors ? "" : "S");
    return 1;
} /* end main() */
