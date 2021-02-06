/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5. The full HDF5 copyright notice, including      *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/*
 * Programmer:    Raymond Lu
 *        13 February 2013
 *
 * Purpose:    Tests the plugin module (H5PL)
 */

#include "h5test.h"
#include "H5srcdir.h"

/*
 * This file needs to access private datatypes from the H5Z and H5PL package.
 */
#define H5PL_PACKAGE
#include "H5PLpkg.h"
#define H5Z_PACKAGE
#include "H5Zpkg.h"

/* Filters for HDF5 internal test */
#define H5Z_FILTER_DYNLIB1 257
#define H5Z_FILTER_DYNLIB2 258
#define H5Z_FILTER_DYNLIB3 259
#define H5Z_FILTER_DYNLIB4 260

const char *FILENAME[] = {"plugin", NULL};
#define FILENAME_BUF_SIZE 1024

/* Dataset names for testing filters */
#define DSET_DEFLATE_NAME "deflate"
#define DSET_DYNLIB1_NAME "dynlib1"
#define DSET_DYNLIB2_NAME "dynlib2"
#define DSET_DYNLIB4_NAME "dynlib4"

/* Parameters for internal filter test */
#define FILTER_CHUNK_DIM1 2
#define FILTER_CHUNK_DIM2 25
#define FILTER_HS_OFFSET1 7
#define FILTER_HS_OFFSET2 30
#define FILTER_HS_SIZE1   4
#define FILTER_HS_SIZE2   50

/* Shared global arrays */
#define DSET_DIM1 100
#define DSET_DIM2 200

/* Limit random number within 20000 */
#define RANDOM_LIMIT 20000

#define GROUP_ITERATION 1000

int points_deflate[DSET_DIM1][DSET_DIM2], points_dynlib1[DSET_DIM1][DSET_DIM2],
    points_dynlib2[DSET_DIM1][DSET_DIM2], points_dynlib4[DSET_DIM1][DSET_DIM2],
    points_bzip2[DSET_DIM1][DSET_DIM2];

/*-------------------------------------------------------------------------
 * Function:  test_filter_internal
 *
 * Purpose:   Tests writing entire data and partial data with filters
 *
 * Return:    Success:    0
 *            Failure:    -1
 *-------------------------------------------------------------------------
 */
static herr_t
test_filter_internal(hid_t fid, const char *name, hid_t dcpl)
{
    herr_t        ret_value    = -1;
    hid_t         dataset      = -1;                     /* Dataset ID */
    hid_t         dxpl         = -1;                     /* Dataset xfer property list ID */
    hid_t         write_dxpl   = -1;                     /* Dataset xfer property list ID for writing */
    hid_t         sid          = -1;                     /* Dataspace ID */
    const hsize_t size[2]      = {DSET_DIM1, DSET_DIM2}; /* Dataspace dimensions */
    const hsize_t hs_offset[2] = {FILTER_HS_OFFSET1, FILTER_HS_OFFSET2}; /* Hyperslab offset */
    const hsize_t hs_size[2]   = {FILTER_HS_SIZE1, FILTER_HS_SIZE2};     /* Hyperslab size */
    void *        tconv_buf    = NULL;                                   /* Temporary conversion buffer */
    int           points[DSET_DIM1][DSET_DIM2], check[DSET_DIM1][DSET_DIM2];
    size_t        i, j; /* Local index variables */
    int           n = 0;

    /* Create the data space */
    if ((sid = H5Screate_simple(2, size, NULL)) < 0)
        TEST_ERROR

    /*
     * Create a small conversion buffer to test strip mining. We
     * might as well test all we can!
     */
    if ((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0)
        TEST_ERROR
    tconv_buf = HDmalloc((size_t)1000);
    if (H5Pset_buffer(dxpl, (size_t)1000, tconv_buf, NULL) < 0)
        TEST_ERROR
    if ((write_dxpl = H5Pcopy(dxpl)) < 0)
        TEST_ERROR;

    TESTING("    filters (setup)");

    /* Check if all the filters are available */
    if (H5Pall_filters_avail(dcpl) != TRUE) {
        H5_FAILED();
        HDprintf("    Line %d: Incorrect filter availability\n", __LINE__);
        TEST_ERROR
    } /* end if */

    /* Create the dataset */
    if ((dataset = H5Dcreate2(fid, name, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR

    PASSED();

    /*----------------------------------------------------------------------
     * STEP 1: Read uninitialized data.  It should be zero.
     *----------------------------------------------------------------------
     */
    TESTING("    filters (uninitialized read)");

    if (H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl, check) < 0)
        TEST_ERROR;

    for (i = 0; i < (size_t)size[0]; i++)
        for (j = 0; j < (size_t)size[1]; j++)
            if (0 != check[i][j]) {
                H5_FAILED();
                HDprintf("    Read a non-zero value.\n");
                HDprintf("    At index %lu,%lu\n", (unsigned long)i, (unsigned long)j);
                TEST_ERROR
            } /* end if */
    PASSED();

    /*----------------------------------------------------------------------
     * STEP 2: Test filters by setting up a chunked dataset and writing
     * to it.
     *----------------------------------------------------------------------
     */
    TESTING("    filters (write)");

    n = 0;
    for (i = 0; i < size[0]; i++)
        for (j = 0; j < size[1]; j++)
            points[i][j] = (int)(n++);

    if (H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, write_dxpl, points) < 0)
        TEST_ERROR;

    PASSED();

    /*----------------------------------------------------------------------
     * STEP 3: Try to read the data we just wrote.
     *----------------------------------------------------------------------
     */
    TESTING("    filters (read)");

    /* Read the dataset back */
    if (H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl, check) < 0)
        TEST_ERROR;

    /* Check that the values read are the same as the values written */
    for (i = 0; i < size[0]; i++)
        for (j = 0; j < size[1]; j++)
            if (points[i][j] != check[i][j]) {
                H5_FAILED();
                HDfprintf(stderr, "    Read different values than written.\n");
                HDfprintf(stderr, "    At index %lu,%lu\n", (unsigned long)i, (unsigned long)j);
                HDfprintf(stderr, "    At original: %d\n", (int)points[i][j]);
                HDfprintf(stderr, "    At returned: %d\n", (int)check[i][j]);
                TEST_ERROR
            } /* end if */

    PASSED();

    /*----------------------------------------------------------------------
     * STEP 4: Write new data over the top of the old data.  The new data is
     * random thus not very compressible, and will cause the chunks to move
     * around as they grow.  We only change values for the left half of the
     * dataset although we rewrite the whole thing.
     *----------------------------------------------------------------------
     */
    TESTING("    filters (modify)");

    for (i = 0; i < size[0]; i++)
        for (j = 0; j < size[1] / 2; j++)
            points[i][j] = (int)HDrandom() % RANDOM_LIMIT;

    if (H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, write_dxpl, points) < 0)
        TEST_ERROR;

    /* Read the dataset back and check it */
    if (H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl, check) < 0)
        TEST_ERROR;

    /* Check that the values read are the same as the values written */
    for (i = 0; i < size[0]; i++)
        for (j = 0; j < size[1]; j++)
            if (points[i][j] != check[i][j]) {
                H5_FAILED();
                HDprintf("    Read different values than written.\n");
                HDprintf("    At index %lu,%lu\n", (unsigned long)i, (unsigned long)j);
                TEST_ERROR
            } /* end if */

    PASSED();

    /*----------------------------------------------------------------------
     * STEP 5: Close the dataset and then open it and read it again.  This
     * insures that the filters message is picked up properly from the
     * object header.
     *----------------------------------------------------------------------
     */
    TESTING("    filters (re-open)");

    if (H5Dclose(dataset) < 0)
        TEST_ERROR;
    if ((dataset = H5Dopen2(fid, name, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if (H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl, check) < 0)
        TEST_ERROR;

    /* Check that the values read are the same as the values written */
    for (i = 0; i < size[0]; i++)
        for (j = 0; j < size[1]; j++)
            if (points[i][j] != check[i][j]) {
                H5_FAILED();
                HDprintf("    Read different values than written.\n");
                HDprintf("    At index %lu,%lu\n", (unsigned long)i, (unsigned long)j);
                TEST_ERROR
            } /* end if */

    PASSED();

    /*----------------------------------------------------------------------
     * STEP 6: Test partial I/O by writing to and then reading from a
     * hyperslab of the dataset.  The hyperslab does not line up on chunk
     * boundaries (we know that case already works from above tests).
     *----------------------------------------------------------------------
     */
    TESTING("    filters (partial I/O)");

    for (i = 0; i < (size_t)hs_size[0]; i++)
        for (j = 0; j < (size_t)hs_size[1]; j++)
            points[(size_t)hs_offset[0] + i][(size_t)hs_offset[1] + j] = (int)HDrandom() % RANDOM_LIMIT;

    if (H5Sselect_hyperslab(sid, H5S_SELECT_SET, hs_offset, NULL, hs_size, NULL) < 0)
        TEST_ERROR;
    /* (Use the "read" DXPL because partial I/O on corrupted data test needs to ignore errors during writing)
     */
    if (H5Dwrite(dataset, H5T_NATIVE_INT, sid, sid, dxpl, points) < 0)
        TEST_ERROR;

    if (H5Dread(dataset, H5T_NATIVE_INT, sid, sid, dxpl, check) < 0)
        TEST_ERROR;

    /* Check that the values read are the same as the values written */
    for (i = 0; i < (size_t)hs_size[0]; i++)
        for (j = 0; j < (size_t)hs_size[1]; j++)
            if (points[(size_t)hs_offset[0] + i][(size_t)hs_offset[1] + j] !=
                check[(size_t)hs_offset[0] + i][(size_t)hs_offset[1] + j]) {
                H5_FAILED();
                HDfprintf(stderr, "    Read different values than written.\n");
                HDfprintf(stderr, "    At index %lu,%lu\n", (unsigned long)((size_t)hs_offset[0] + i),
                          (unsigned long)((size_t)hs_offset[1] + j));
                HDfprintf(stderr, "    At original: %d\n",
                          (int)points[(size_t)hs_offset[0] + i][(size_t)hs_offset[1] + j]);
                HDfprintf(stderr, "    At returned: %d\n",
                          (int)check[(size_t)hs_offset[0] + i][(size_t)hs_offset[1] + j]);
                TEST_ERROR
            } /* end if */

    PASSED();

    /* Save the data written to the file for later comparison when the file
     * is reopened for read test */
    for (i = 0; i < size[0]; i++)
        for (j = 0; j < size[1]; j++)
            if (!HDstrcmp(name, DSET_DEFLATE_NAME))
                points_deflate[i][j] = points[i][j];
            else if (!HDstrcmp(name, DSET_DYNLIB1_NAME))
                points_dynlib1[i][j] = points[i][j];
            else if (!HDstrcmp(name, DSET_DYNLIB2_NAME))
                points_dynlib2[i][j] = points[i][j];
            else if (!HDstrcmp(name, DSET_DYNLIB4_NAME))
                points_dynlib4[i][j] = points[i][j];

    ret_value = 0;

error:
    /* Clean up objects used for this test */
    H5E_BEGIN_TRY
    {
        H5Dclose(dataset);
        H5Sclose(sid);
        H5Pclose(dxpl);
    }
    H5E_END_TRY

    if (tconv_buf)
        HDfree(tconv_buf);
    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:  test_filters_for_datasets
 *
 * Purpose:   Tests creating datasets and writing data with dynamically loaded filters
 *
 * Return:    Success:    0
 *            Failure:    -1
 *-------------------------------------------------------------------------
 */
static herr_t
test_filters_for_datasets(hid_t file)
{
    herr_t        ret_value      = -1;
    hid_t         dc             = -1; /* Dataset creation property list ID */
    const hsize_t chunk_size[2]  = {FILTER_CHUNK_DIM1, FILTER_CHUNK_DIM2}; /* Chunk dimensions */
    unsigned int  compress_level = 9;
    unsigned int  dynlib4_values[4];

    /*----------------------------------------------------------
     * STEP 1: Test deflation by itself.
     *----------------------------------------------------------
     */
    HDputs("Testing deflate filter");
#ifdef H5_HAVE_FILTER_DEFLATE
    if ((dc = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR
    if (H5Pset_chunk(dc, 2, chunk_size) < 0)
        TEST_ERROR
    if (H5Pset_deflate(dc, 6) < 0)
        TEST_ERROR

    if (test_filter_internal(file, DSET_DEFLATE_NAME, dc) < 0)
        TEST_ERROR
    /* Clean up objects used for this test */
    if (H5Pclose(dc) < 0)
        TEST_ERROR
#else  /* H5_HAVE_FILTER_DEFLATE */
    SKIPPED();
    HDputs("    Deflate filter not enabled");
#endif /* H5_HAVE_FILTER_DEFLATE */

    /*----------------------------------------------------------
     * STEP 2: Test DYNLIB1 by itself.
     *----------------------------------------------------------
     */
    HDputs("    DYNLIB1 filter");
    if ((dc = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR
    if (H5Pset_chunk(dc, 2, chunk_size) < 0)
        TEST_ERROR
    if (H5Pset_filter(dc, H5Z_FILTER_DYNLIB1, H5Z_FLAG_MANDATORY, (size_t)1, &compress_level) < 0)
        TEST_ERROR

    if (test_filter_internal(file, DSET_DYNLIB1_NAME, dc) < 0)
        TEST_ERROR

    /* Clean up objects used for this test */
    if (H5Pclose(dc) < 0)
        TEST_ERROR

    /* Unregister the dynamic filter DYNLIB1 for testing purpose. The next time when this test is run for
     * the new file format, the library's H5PL code has to search in the table of loaded plugin libraries
     * for this filter. */
    if (H5Zunregister(H5Z_FILTER_DYNLIB1) < 0)
        TEST_ERROR

    /*----------------------------------------------------------
     * STEP 3: Test DYNLIB2 by itself.
     *----------------------------------------------------------
     */
    HDputs("    DYNLIB2 filter");
    if ((dc = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR
    if (H5Pset_chunk(dc, 2, chunk_size) < 0)
        TEST_ERROR
    if (H5Pset_filter(dc, H5Z_FILTER_DYNLIB2, H5Z_FLAG_MANDATORY, 0, NULL) < 0)
        TEST_ERROR

    if (test_filter_internal(file, DSET_DYNLIB2_NAME, dc) < 0)
        TEST_ERROR

    /* Clean up objects used for this test */
    if (H5Pclose(dc) < 0)
        TEST_ERROR

    /* Unregister the dynamic filter DYNLIB2 for testing purpose. The next time when this test is run for
     * the new file format, the library's H5PL code has to search in the table of loaded plugin libraries
     * for this filter. */
    if (H5Zunregister(H5Z_FILTER_DYNLIB2) < 0)
        TEST_ERROR

    /*----------------------------------------------------------
     * STEP 4: Test DYNLIB4 by itself.
     *----------------------------------------------------------
     */
    HDputs("    DYNLIB4 filter");
    if ((dc = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR
    if (H5Pset_chunk(dc, 2, chunk_size) < 0)
        TEST_ERROR
    dynlib4_values[0] = 9;
    if (H5get_libversion(&dynlib4_values[1], &dynlib4_values[2], &dynlib4_values[3]) < 0)
        TEST_ERROR
    if (H5Pset_filter(dc, H5Z_FILTER_DYNLIB4, H5Z_FLAG_MANDATORY, (size_t)4, dynlib4_values) < 0)
        TEST_ERROR

    if (test_filter_internal(file, DSET_DYNLIB4_NAME, dc) < 0)
        TEST_ERROR

    /* Clean up objects used for this test */
    if (H5Pclose(dc) < 0)
        TEST_ERROR

    /* Unregister the dynamic filter DYNLIB4 for testing purpose. The next time when this test is run for
     * the new file format, the library's H5PL code has to search in the table of loaded plugin libraries
     * for this filter. */
    if (H5Zunregister(H5Z_FILTER_DYNLIB4) < 0)
        TEST_ERROR

    ret_value = 0;

error:
    /* Clean up objects used for this test */
    H5E_BEGIN_TRY { H5Pclose(dc); }
    H5E_END_TRY

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:  test_read_data
 *
 * Purpose:   Tests reading data and compares values
 *
 * Return:    Success:    0
 *            Failure:    -1
 *-------------------------------------------------------------------------
 */
static herr_t
test_read_data(hid_t dataset, int *origin_data)
{
    herr_t        ret_value = -1;
    int           check[DSET_DIM1][DSET_DIM2];
    const hsize_t size[2] = {DSET_DIM1, DSET_DIM2}; /* Dataspace dimensions */
    int *         data_p  = origin_data;
    size_t        i, j; /* Local index variables */

    /* Read the dataset back */
    if (H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, check) < 0)
        TEST_ERROR;

    /* Check that the values read are the same as the values written */
    for (i = 0; i < size[0]; i++)
        for (j = 0; j < size[1]; j++) {
            if (*data_p != check[i][j]) {
                H5_FAILED();
                HDfprintf(stderr, "    Read different values than written.\n");
                HDfprintf(stderr, "    At index %lu,%lu\n", (unsigned long)i, (unsigned long)j);
                HDfprintf(stderr, "    At original: %d\n", *data_p);
                HDfprintf(stderr, "    At returned: %d\n", (int)check[i][j]);
                TEST_ERROR
            } /* end if */
            data_p++;
        }

    PASSED();
    ret_value = 0;

error:
    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:  test_read_with_filters
 *
 * Purpose:   Tests reading dataset created with dynamically loaded filters
 *
 * Return:    Success:    0
 *            Failure:    -1
 *-------------------------------------------------------------------------
 */
static herr_t
test_read_with_filters(hid_t file)
{
    herr_t ret_value = -1;
    hid_t  dset      = -1; /* Dataset ID */

    /*----------------------------------------------------------
     * STEP 1: Test deflation by itself.
     *----------------------------------------------------------
     */
    TESTING("deflate filter");

#ifdef H5_HAVE_FILTER_DEFLATE
    if (H5Zfilter_avail(H5Z_FILTER_DEFLATE) != TRUE)
        TEST_ERROR

    if ((dset = H5Dopen2(file, DSET_DEFLATE_NAME, H5P_DEFAULT)) < 0)
        TEST_ERROR

    if (test_read_data(dset, (int *)points_deflate) < 0)
        TEST_ERROR

    if (H5Dclose(dset) < 0)
        TEST_ERROR

        /* Clean up objects used for this test */
#else  /* H5_HAVE_FILTER_DEFLATE */
    SKIPPED();
    HDputs("    Deflate filter not enabled");
#endif /* H5_HAVE_FILTER_DEFLATE */

    /*----------------------------------------------------------
     * STEP 2: Test DYNLIB1 by itself.
     *----------------------------------------------------------
     */
    TESTING("    DYNLIB1 filter");

    if ((dset = H5Dopen2(file, DSET_DYNLIB1_NAME, H5P_DEFAULT)) < 0)
        TEST_ERROR

    if (test_read_data(dset, (int *)points_dynlib1) < 0)
        TEST_ERROR

    if (H5Dclose(dset) < 0)
        TEST_ERROR

    /*----------------------------------------------------------
     * STEP 3: Test Bogus2 by itself.
     *----------------------------------------------------------
     */
    TESTING("    DYNLIB2 filter");

    if ((dset = H5Dopen2(file, DSET_DYNLIB2_NAME, H5P_DEFAULT)) < 0)
        TEST_ERROR

    if (test_read_data(dset, (int *)points_dynlib2) < 0)
        TEST_ERROR

    if (H5Dclose(dset) < 0)
        TEST_ERROR

    /*----------------------------------------------------------
     * STEP 4: Test DYNLIB4 by itself.
     *----------------------------------------------------------
     */
    TESTING("    DYNLIB4 filter");

    if ((dset = H5Dopen2(file, DSET_DYNLIB4_NAME, H5P_DEFAULT)) < 0)
        TEST_ERROR

    if (test_read_data(dset, (int *)points_dynlib4) < 0)
        TEST_ERROR

    if (H5Dclose(dset) < 0)
        TEST_ERROR

    ret_value = 0;

error:
    /* Clean up objects used for this test */
    H5E_BEGIN_TRY { H5Dclose(dset); }
    H5E_END_TRY

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:  test_noread_data
 *
 * Purpose:   Tests not reading data
 *
 * Return:    Success:    0
 *            Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_noread_data(hid_t dataset)
{
    herr_t ret_value = -1;
    int    check[DSET_DIM1][DSET_DIM2];
    herr_t ret = -1;

    /* Read the dataset back */
    H5E_BEGIN_TRY { ret = H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, check); }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR

    PASSED();
    ret_value = 0;

error:
    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:  test_noread_with_filters
 *
 * Purpose:   Tests reading dataset created with dynamically loaded filters disabled
 *
 * Return:    Success:    0
 *            Failure:    -1
 *-------------------------------------------------------------------------
 */
static herr_t
test_noread_with_filters(hid_t file)
{
    herr_t   ret_value = -1;
    hid_t    dset      = -1; /* Dataset ID */
    unsigned plugin_state;   /* status of plugins */

    TESTING("DYNLIB1 filter with plugins disabled");

    /* disable filter plugin */
    if (H5PLget_loading_state(&plugin_state) < 0)
        TEST_ERROR
    plugin_state = plugin_state & ~H5PL_FILTER_PLUGIN;
    if (H5PLset_loading_state(plugin_state) < 0)
        TEST_ERROR

    if ((dset = H5Dopen2(file, DSET_DYNLIB1_NAME, H5P_DEFAULT)) < 0)
        TEST_ERROR

    if (test_noread_data(dset) < 0)
        TEST_ERROR

    if (H5Dclose(dset) < 0)
        TEST_ERROR

    ret_value = 0;

error:
    /* re-enable filter plugin */
    plugin_state = plugin_state | H5PL_FILTER_PLUGIN;
    H5PLset_loading_state(plugin_state);

    /* Clean up objects used for this test */
    H5E_BEGIN_TRY { H5Dclose(dset); }
    H5E_END_TRY

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:  test_filters_for_groups
 *
 * Purpose:   Tests creating group with dynamically loaded filters
 *
 * Return:    Success:    0
 *            Failure:    -1
 *-------------------------------------------------------------------------
 */
static herr_t
test_filters_for_groups(hid_t file)
{
    herr_t ret_value = -1;
    hid_t  gcpl      = -1;
    hid_t  gid       = -1;
    hid_t  group     = -1;
    int    i;
    char   gname[256];

    TESTING("DYNLIB3 filter for group");

    if ((gcpl = H5Pcreate(H5P_GROUP_CREATE)) < 0)
        TEST_ERROR

    /* Use DYNLIB3 for creating groups */
    if (H5Pset_filter(gcpl, H5Z_FILTER_DYNLIB3, H5Z_FLAG_MANDATORY, (size_t)0, NULL) < 0)
        TEST_ERROR

    /* Create a group using this filter */
    if ((gid = H5Gcreate2(file, "group1", H5P_DEFAULT, gcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Create multiple groups under "group1" */
    for (i = 0; i < GROUP_ITERATION; i++) {
        HDsprintf(gname, "group_%d", i);
        if ((group = H5Gcreate2(gid, gname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
            TEST_ERROR
        if (H5Gclose(group) < 0)
            TEST_ERROR
    }

    /* Close the group */
    if (H5Gclose(gid) < 0)
        TEST_ERROR

    /* Clean up objects used for this test */
    if (H5Pclose(gcpl) < 0)
        TEST_ERROR

    PASSED();

    ret_value = 0;

error:
    /* Clean up objects used for this test */
    H5E_BEGIN_TRY
    {
        H5Gclose(group);
        H5Gclose(gid);
        H5Pclose(gcpl);
    }
    H5E_END_TRY

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:    test_groups_with_filters
 *
 * Purpose:    Tests opening group with dynamically loaded filters
 *
 * Return:    Success:    0
 *        Failure:    -1
 *
 * Programmer:    Raymond Lu
 *              1 April 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_groups_with_filters(hid_t file)
{
    herr_t ret_value = -1;
    hid_t  gid;
    hid_t  group;
    int    i;
    char   gname[256];

    TESTING("opening groups with DYNLIB3 filter");

    /* Open the top group */
    if ((gid = H5Gopen2(file, "group1", H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Create multiple groups under "group1" */
    for (i = 0; i < GROUP_ITERATION; i++) {
        HDsprintf(gname, "group_%d", i);
        if ((group = H5Gopen2(gid, gname, H5P_DEFAULT)) < 0)
            TEST_ERROR
        if (H5Gclose(group) < 0)
            TEST_ERROR
    }

    /* Close the group */
    if (H5Gclose(gid) < 0)
        TEST_ERROR

    PASSED();

    ret_value = 0;

error:
    /* Clean up objects used for this test */
    H5E_BEGIN_TRY
    {
        H5Gclose(group);
        H5Gclose(gid);
    }
    H5E_END_TRY

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:  test_filter_path_apis
 *
 * Purpose:   Tests accessing the path table for dynamically loaded filters
 *
 * Return:    Success:    0
 *            Failure:    -1
 *-------------------------------------------------------------------------
 */
static herr_t
test_filter_path_apis(void)
{
    herr_t       ret_value = -1;
    unsigned int i;
    unsigned int ndx;
    herr_t       ret;
    ssize_t      pathlen = -1;
    char         pathname[256];
    char         tempname[256];

    HDputs("Testing access to the filter path table");

    if (H5Zfilter_avail(H5Z_FILTER_DYNLIB1) != TRUE)
        TEST_ERROR

    TESTING("    initialize");
    H5PLsize(&ndx);
    if (ndx != 2)
        TEST_ERROR
    PASSED();

    TESTING("    remove");
    /* Remove all existing paths*/
    for (i = ndx; i > 0; i--)
        if (H5PLremove(i - 1) < 0) {
            HDfprintf(stderr, "    at %d: %s\n", i, pathname);
            TEST_ERROR
        } /* end if */
    /* Verify the table is empty */
    H5PLsize(&ndx);
    if (ndx > 0)
        TEST_ERROR
    PASSED();

    TESTING("    remove (exceed min)");
    /* Exceed the min path removal */
    H5E_BEGIN_TRY { ret = H5PLremove(0); }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR
    PASSED();

    TESTING("    append");
    /* Create multiple paths to fill table */
    for (i = 0; i < H5PL_MAX_PATH_NUM; i++) {
        HDsprintf(pathname, "a_path_%d", i);
        if (H5PLappend(pathname) < 0) {
            HDfprintf(stderr, "    at %d: %s\n", i, pathname);
            TEST_ERROR
        }
    }
    /* Verify the table is full */
    H5PLsize(&ndx);
    if (ndx != H5PL_MAX_PATH_NUM)
        TEST_ERROR
    PASSED();

    TESTING("    append (exceed)");
    /* Exceed the max path append */
    H5E_BEGIN_TRY
    {
        HDsprintf(pathname, "a_path_%d", H5PL_MAX_PATH_NUM);
        ret = H5PLappend(pathname);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR
    PASSED();

    TESTING("    remove (exceed max)");
    /* Exceed the max path removal */
    H5E_BEGIN_TRY { ret = H5PLremove(H5PL_MAX_PATH_NUM); }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR
    PASSED();

    TESTING("    get (path name)");
    if ((pathlen = H5PLget(0, NULL, 0)) <= 0) {
        HDfprintf(stderr, "    get path 0 length failed\n");
        TEST_ERROR
    }
    if (pathlen != 8)
        TEST_ERROR

    if ((pathlen = H5PLget(0, pathname, 256)) <= 0) {
        HDfprintf(stderr, "    get 0 len: %d : %s\n", pathlen, pathname);
        TEST_ERROR
    }
    if (HDstrcmp(pathname, "a_path_0") != 0) {
        HDfprintf(stderr, "    get 0: %s\n", pathname);
        TEST_ERROR
    }
    PASSED();

    TESTING("    get (bounds)");
    if ((pathlen = H5PLget(1, pathname, 256)) <= 0)
        TEST_ERROR
    if (HDstrcmp(pathname, "a_path_1") != 0) {
        HDfprintf(stderr, "    get 1: %s\n", pathname);
        TEST_ERROR
    }
    if ((pathlen = H5PLget(H5PL_MAX_PATH_NUM - 1, pathname, 256)) <= 0)
        TEST_ERROR
    HDsprintf(tempname, "a_path_%d", H5PL_MAX_PATH_NUM - 1);
    if (HDstrcmp(pathname, tempname) != 0) {
        HDfprintf(stderr, "    get %d: %s\n", H5PL_MAX_PATH_NUM - 1, pathname);
        TEST_ERROR
    }
    PASSED();

    TESTING("    get (bounds exceed)");
    H5E_BEGIN_TRY { pathlen = H5PLget(H5PL_MAX_PATH_NUM, NULL, 0); }
    H5E_END_TRY
    if (pathlen > 0)
        TEST_ERROR
    PASSED();

    TESTING("    remove (verify for prepend)");
    /* Remove one path*/
    if (H5PLremove(8) < 0)
        TEST_ERROR

    /* Verify that the entries were moved */
    if ((pathlen = H5PLget(8, pathname, 256)) <= 0)
        TEST_ERROR
    if (HDstrcmp(pathname, "a_path_9") != 0) {
        HDfprintf(stderr, "    get 8: %s\n", pathname);
        TEST_ERROR
    }
    PASSED();

    /* Verify the table is not full */
    H5PLsize(&ndx);
    if (ndx != H5PL_MAX_PATH_NUM - 1)
        TEST_ERROR

    TESTING("    prepend");
    /* Prepend one path*/
    HDsprintf(pathname, "a_path_%d", H5PL_MAX_PATH_NUM + 1);
    if (H5PLprepend(pathname) < 0) {
        HDfprintf(stderr, "    prepend %d: %s\n", H5PL_MAX_PATH_NUM + 1, pathname);
        TEST_ERROR
    }

    /* Verify the table is full */
    H5PLsize(&ndx);
    if (ndx != H5PL_MAX_PATH_NUM)
        TEST_ERROR

    /* Verify that the entries were moved */
    if (H5PLget(8, pathname, 256) <= 0)
        TEST_ERROR
    if (HDstrcmp(pathname, "a_path_7") != 0) {
        HDfprintf(stderr, "    get 8: %s\n", pathname);
        TEST_ERROR
    }
    if (H5PLget(0, pathname, 256) <= 0)
        TEST_ERROR
    HDsprintf(tempname, "a_path_%d", H5PL_MAX_PATH_NUM + 1);
    if (HDstrcmp(pathname, tempname) != 0) {
        HDfprintf(stderr, "    get 0: %s\n", pathname);
        TEST_ERROR
    }
    PASSED();

    TESTING("    prepend (exceed)");
    /* Exceed the max path prepend */
    H5E_BEGIN_TRY
    {
        HDsprintf(pathname, "a_path_%d", H5PL_MAX_PATH_NUM + 2);
        ret = H5PLprepend(pathname);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR
    PASSED();

    TESTING("    replace");
    /* Replace one path*/
    HDsprintf(pathname, "a_path_%d", H5PL_MAX_PATH_NUM + 4);
    if (H5PLreplace(pathname, 1) < 0) {
        HDfprintf(stderr, "    replace 1: %s\n", pathname);
        TEST_ERROR
    }

    /* Verify the table is full */
    H5PLsize(&ndx);
    if (ndx != H5PL_MAX_PATH_NUM)
        TEST_ERROR

    /* Verify that the entries were not moved */
    if (H5PLget(0, pathname, 256) <= 0)
        TEST_ERROR
    HDsprintf(tempname, "a_path_%d", H5PL_MAX_PATH_NUM + 1);
    if (HDstrcmp(pathname, tempname) != 0) {
        HDfprintf(stderr, "    get 0: %s\n", pathname);
        TEST_ERROR
    }
    if (H5PLget(2, pathname, 256) <= 0)
        TEST_ERROR
    if (HDstrcmp(pathname, "a_path_1") != 0) {
        HDfprintf(stderr, "    get 2: %s\n", pathname);
        TEST_ERROR
    }
    PASSED();

    TESTING("    remove (verify for insert)");
    /* Remove one path*/
    if (H5PLremove(4) < 0)
        TEST_ERROR

    /* Verify that the entries were moved */
    if (H5PLget(4, pathname, 256) <= 0)
        TEST_ERROR
    if (HDstrcmp(pathname, "a_path_4") != 0) {
        HDfprintf(stderr, "    get 4: %s\n", pathname);
        TEST_ERROR
    }
    PASSED();

    /* Verify the table is not full */
    H5PLsize(&ndx);
    if (ndx != 15)
        TEST_ERROR

    TESTING("    insert");
    /* Insert one path*/
    HDsprintf(pathname, "a_path_%d", H5PL_MAX_PATH_NUM + 5);
    if (H5PLinsert(pathname, 3) < 0) {
        HDfprintf(stderr, "    insert 3: %s\n", pathname);
        TEST_ERROR
    }

    /* Verify that the entries were moved */
    if (H5PLget(4, pathname, 256) <= 0)
        TEST_ERROR
    if (HDstrcmp(pathname, "a_path_2") != 0) {
        HDfprintf(stderr, "    get 4: %s\n", pathname);
        TEST_ERROR
    }
    PASSED();

    /* Verify the table is full */
    H5PLsize(&ndx);
    if (ndx != H5PL_MAX_PATH_NUM)
        TEST_ERROR

    TESTING("    insert (exceed)");
    /* Exceed the max path insert */
    H5E_BEGIN_TRY
    {
        HDsprintf(pathname, "a_path_%d", H5PL_MAX_PATH_NUM + 6);
        ret = H5PLinsert(pathname, 12);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR

    PASSED();

    ret_value = 0;

error:
    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:    Tests the plugin module (H5PL)
 *
 * Return:    Success:    exit(EXIT_SUCCESS)
 *
 *        Failure:    exit(EXIT_FAILURE)
 *
 * Programmer:    Raymond Lu
 *        14 March 2013
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    char     filename[FILENAME_BUF_SIZE];
    hid_t    file  = -1;
    hid_t    fapl  = -1;
    hid_t    fapl2 = -1;
    unsigned new_format;
    int      mdc_nelmts;
    size_t   rdcc_nelmts;
    size_t   rdcc_nbytes;
    double   rdcc_w0;
    int      nerrors = 0;

    /* Testing setup */
    h5_reset();
    fapl = h5_fileaccess();

    /* Turn off the chunk cache, so all the chunks are immediately written to disk */
    if (H5Pget_cache(fapl, &mdc_nelmts, &rdcc_nelmts, &rdcc_nbytes, &rdcc_w0) < 0)
        TEST_ERROR
    rdcc_nbytes = 0;
    if (H5Pset_cache(fapl, mdc_nelmts, rdcc_nelmts, rdcc_nbytes, rdcc_w0) < 0)
        TEST_ERROR

    /* Copy the file access property list */
    if ((fapl2 = H5Pcopy(fapl)) < 0)
        TEST_ERROR

    /* Set the "use the latest version of the format" bounds for creating objects in the file */
    if (H5Pset_libver_bounds(fapl2, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        TEST_ERROR

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Test with old & new format groups */
    for (new_format = FALSE; new_format <= TRUE; new_format++) {
        hid_t my_fapl;

        /* Set the FAPL for the type of format */
        if (new_format) {
            HDputs("\nTesting with new file format:");
            my_fapl = fapl2;
        } /* end if */
        else {
            HDputs("Testing with old file format:");
            my_fapl = fapl;
        } /* end else */

        /* Create the file for this test */
        if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, my_fapl)) < 0)
            TEST_ERROR

        /* Test dynamically loaded filters for chunked dataset */
        nerrors += (test_filters_for_datasets(file) < 0 ? 1 : 0);

        /* Test dynamically loaded filters for groups */
        nerrors += (test_filters_for_groups(file) < 0 ? 1 : 0);

        if (H5Fclose(file) < 0)
            TEST_ERROR
    } /* end for */

    /* Close FAPL */
    if (H5Pclose(fapl2) < 0)
        TEST_ERROR
    if (H5Pclose(fapl) < 0)
        TEST_ERROR

    /* Restore the default error handler (set in h5_reset()) */
    h5_restore_err();

    HDputs("\nTesting reading data with with dynamic plugin filters:");

    /* Close the library so that all loaded plugin libraries are unloaded */
    h5_reset();
    fapl = h5_fileaccess();

    /* Reopen the file for testing data reading */
    if ((file = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
        TEST_ERROR

    /* Read the data with filters */
    nerrors += (test_read_with_filters(file) < 0 ? 1 : 0);

    /* Open the groups with filters */
    nerrors += (test_groups_with_filters(file) < 0 ? 1 : 0);

    /* Restore the default error handler (set in h5_reset()) */
    h5_restore_err();

    /* Close the library so that all loaded plugin libraries are unloaded */
    h5_reset();
    fapl = h5_fileaccess();

    /* Reopen the file for testing data reading */
    if ((file = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
        TEST_ERROR

    /* Read the data with disabled filters */
    nerrors += (test_noread_with_filters(file) < 0 ? 1 : 0);

    if (H5Fclose(file) < 0)
        TEST_ERROR

    /* Test the APIs for access to the filter plugin path table */
    nerrors += (test_filter_path_apis() < 0 ? 1 : 0);

    if (nerrors)
        TEST_ERROR

    HDprintf("All plugin tests passed.\n");
    h5_cleanup(FILENAME, fapl);

    return 0;

error:
    nerrors = MAX(1, nerrors);
    HDprintf("***** %d PLUGIN TEST%s FAILED! *****\n", nerrors, 1 == nerrors ? "" : "S");
    return 1;
}
