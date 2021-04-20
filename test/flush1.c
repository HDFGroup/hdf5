/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
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
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Friday, October 23, 1998
 *
 * Purpose:    This is the first half of a two-part test that makes sure
 *        that a file can be read after an application crashes as long
 *        as the file was flushed first.  We simulate a crash by
 *        calling _exit(EXIT_SUCCESS) since this doesn't flush HDF5 caches but
 *        still exits with success.
 */
#include "h5test.h"

const char *FILENAME[] = {"flush", "noflush", "noflush_extend", NULL};

static double the_data[100][100];

/*-------------------------------------------------------------------------
 * Function:    create_file
 *
 * Purpose:     Creates files and datasets used in part 1 of the test
 *
 * Return:      Success:    a valid file ID
 *              Failure:    -1
 *
 * Programmer:    Leon Arber
 *              Sept. 26, 2006
 *
 *-------------------------------------------------------------------------
 */
static hid_t
create_file(const char *filename, hid_t fapl_id)
{
    hid_t   fid = -1; /* file ID                          */
    hid_t   dcpl, space, dset, groups, grp;
    hsize_t ds_size[2] = {100, 100};
    hsize_t ch_size[2] = {5, 5};
    size_t  i, j;

    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id)) < 0)
        STACK_ERROR

    /* Create a chunked dataset */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR
    if (H5Pset_chunk(dcpl, 2, ch_size) < 0)
        FAIL_STACK_ERROR
    if ((space = H5Screate_simple(2, ds_size, NULL)) < 0)
        FAIL_STACK_ERROR
    if ((dset = H5Dcreate2(fid, "dset", H5T_NATIVE_FLOAT, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Write some data */
    for (i = 0; i < ds_size[0]; i++)
        /*
         * The extra cast in the following statement is a bug workaround
         * for the Win32 version 5.0 compiler.
         * 1998-11-06 ptl
         */
        for (j = 0; j < (size_t)ds_size[1]; j++)
            the_data[i][j] = (double)(hssize_t)i / (hssize_t)(j + 1);
    if (H5Dwrite(dset, H5T_NATIVE_DOUBLE, space, space, H5P_DEFAULT, the_data) < 0)
        FAIL_STACK_ERROR

    /* Create some groups */
    if ((groups = H5Gcreate2(fid, "some_groups", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR
    for (i = 0; i < 100; i++) {
        HDsprintf(filename, "grp%02u", (unsigned)i);
        if ((grp = H5Gcreate2(groups, filename, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR
        if (H5Gclose(grp) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    return fid;

error:
    HD_exit(1);
}

/*-------------------------------------------------------------------------
 * Function:    extend_file
 *
 * Purpose:    Add a small dataset to the file.
 *
 * Return:    Success:    0
 *
 *        Failure:    1
 *
 * Programmer:    Leon Arber
 *              Oct. 4, 2006
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static hid_t
extend_file(hid_t file)
{
    hid_t   dcpl, space, dset;
    hsize_t ds_size[2] = {100, 100};
    hsize_t ch_size[2] = {5, 5};
    size_t  i, j;

    /* Create a chunked dataset */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;
    if (H5Pset_chunk(dcpl, 2, ch_size) < 0)
        goto error;
    if ((space = H5Screate_simple(2, ds_size, NULL)) < 0)
        goto error;
    if ((dset = H5Dcreate2(file, "dset2", H5T_NATIVE_FLOAT, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) <
        0)
        goto error;

    /* Write some data */
    for (i = 0; i < ds_size[0]; i++) {
        /*
         * The extra cast in the following statement is a bug workaround
         * for the Win32 version 5.0 compiler.
         * 1998-11-06 ptl
         */
        for (j = 0; j < (size_t)ds_size[1]; j++) {
            the_data[i][j] = (double)(hssize_t)i / (hssize_t)(j + 1);
        }
    }
    if (H5Dwrite(dset, H5T_NATIVE_DOUBLE, space, space, H5P_DEFAULT, the_data) < 0)
        goto error;

    return file;

error:
    HD_exit(1);
}

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:    Part 1 of a two-part H5Fflush() test.
 *
 * Return:    Success:    0
 *
 *        Failure:    1
 *
 * Programmer:    Robb Matzke
 *              Friday, October 23, 1998
 *
 * Modifications:
 *         Leon Arber
 *         Sept. 26, 2006, expand test to check for failure if H5Fflush is not called.
 *         Oct. 4 2006, expand test to check for partial failure in case file is flushed, but then
 *                 new datasets are created after the flush.
 *
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t fid     = -1;   /* file ID                                  */
    hid_t fapl_id = -1;   /* file access proplist ID                  */
    char  filename[1024]; /* filename                                 */

    h5_reset();
    if ((fapl_id = h5_fileaccess()) < 0)
        TEST_ERROR

    /*************************************************/
    /* NOTE: Not closing the file ID is intentional! */
    /*************************************************/

    /* Create a file and flush */
    TESTING("H5Fflush (part1 with flush)");

    /* Create the file */
    h5_fixname(FILENAME[0], fapl_id, filename, sizeof(filename));
    if ((fid = create_file(filename, fapl_id)) < 0)
        TEST_ERROR
    if (H5Fflush(fid, H5F_SCOPE_GLOBAL) < 0)
        FAIL_STACK_ERROR
    PASSED();

    /* Create a file which will not be flushed */
    TESTING("H5Fflush (part1 without flush)");
    h5_fixname(FILENAME[2], fapl_id, filename, sizeof(filename));
    if ((fid = create_file(filename, fapl_id)) < 0)
        TEST_ERROR
    PASSED();

    /* Flush and exit without closing the library */
    if (H5Fflush(fid, H5F_SCOPE_GLOBAL) < 0)
        goto error;
    /* Add a bit to the file and don't flush the new part */
    extend_file(fid);

    /* Create the other file which will not be flushed */
    h5_fixname(FILENAME[1], fapl_id, filename, sizeof(filename));
    if ((fid = create_file(filename, fapl_id)) < 0)
        TEST_ERROR
    PASSED();

    /* Flush console output streams */
    HDfflush(stdout);
    HDfflush(stderr);

    /* DO NOT CLOSE FILE ID! */
    if (H5Pclose(fapl_id) < 0)
        STACK_ERROR

    /* _exit() is necessary since we want a hard close of the library */
    HD_exit(EXIT_SUCCESS);

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(fapl_id);
    }
    H5E_END_TRY;

    HDexit(EXIT_FAILURE);
} /* end main() */
