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
 * Programmer:  Robb Matzke
 *              Thursday, July 30, 1998
 *
 * Purpose:    Determines if the modification time message is working
 *        properly.  Specifically, the code in H5O_mtime_decode() is
 *        very OS-dependent and this test tries to figure out if it's
 *        working properly.
 */
#include "h5test.h"
#include "H5srcdir.h"

const char *FILENAME[] = {"mtime", NULL};

#define TESTFILE1 "tmtimeo.h5"
#define MTIME1    1055531866
#define TESTFILE2 "tmtimen.h5"
#define MTIME2    1041606478

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:    H5O_mtime_decode() test.
 *
 * Return:      EXIT_SUCCESS/EXIT_FAILURE
 *
 * Programmer:    Robb Matzke
 *              Thursday, July 30, 1998
 *
 * Modifications:
 *              Added checks for old and new modification time messages
 *              in pre-created datafiles (generated with gen_old_mtime.c and
 *              gen_new_mtime.c).
 *              Quincey Koziol
 *              Friday, January  3, 2003
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t       fapl, file, space, dset;
    hsize_t     size[1] = {2};
    time_t      now;
    struct tm * tm;
    H5O_info2_t oi1, oi2;
    signed char buf1[32], buf2[32];
    char        filename[1024];
    int         token_cmp;

    h5_reset();
    fapl = h5_fileaccess();

    TESTING("modification time messages");

    /* Create the file, create a dataset, then close the file */
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;
    if ((space = H5Screate_simple(1, size, NULL)) < 0)
        TEST_ERROR;
    if ((dset = H5Dcreate2(file, "dset", H5T_NATIVE_SCHAR, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;
    now = HDtime(NULL);
    if (H5Dclose(dset) < 0)
        TEST_ERROR;
    if (H5Sclose(space) < 0)
        TEST_ERROR;
    if (H5Fclose(file) < 0)
        TEST_ERROR;

    /*
     * Open the file and get the modification time. We'll test the
     * H5Oget_info() arguments too: being able to stat something without
     * knowing its name.
     */
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if ((file = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
        TEST_ERROR;
    if (H5Oget_info_by_name3(file, "dset", &oi1, H5O_INFO_BASIC | H5O_INFO_TIME, H5P_DEFAULT) < 0)
        TEST_ERROR;
    if ((dset = H5Dopen2(file, "dset", H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if (H5Oget_info3(dset, &oi2, H5O_INFO_BASIC | H5O_INFO_TIME) < 0)
        TEST_ERROR;
    if (H5Otoken_cmp(file, &oi1.token, &oi2.token, &token_cmp) < 0)
        TEST_ERROR;
    if (H5Dclose(dset) < 0)
        TEST_ERROR;
    if (H5Fclose(file) < 0)
        TEST_ERROR;

    /* Compare object tokens & times from the two ways of calling H5Oget_info() */
    if (token_cmp || oi1.ctime != oi2.ctime) {
        H5_FAILED();
        HDputs("    Calling H5Oget_info() with the dataset ID returned");
        HDputs("    different values than calling it with a file and dataset");
        HDputs("    name.");
        goto error;
    }

    /* Compare times -- they must be within 60 seconds of one another */
    if (0 == oi1.ctime) {
        SKIPPED();
        HDputs("    The modification time could not be decoded on this OS.");
        HDputs("    Modification times will be mantained in the file but");
        HDputs("    cannot be queried on this system.  See H5O_mtime_decode().");
        return 0;
    }
    else if (HDfabs(HDdifftime(now, oi1.ctime)) > (double)60.0F) {
        H5_FAILED();
        tm = HDlocaltime(&(oi1.ctime));
        HDstrftime((char *)buf1, sizeof buf1, "%Y-%m-%d %H:%M:%S", tm);
        tm = HDlocaltime(&now);
        HDstrftime((char *)buf2, sizeof buf2, "%Y-%m-%d %H:%M:%S", tm);
        HDprintf("    got: %s\n    ans: %s\n", buf1, buf2);
        goto error;
    }
    PASSED();

    /* Check opening existing file with old-style modification time information
     * and make certain that the time is correct
     */
    TESTING("accessing old modification time messages");

    {
        const char *testfile = H5_get_srcdir_filename(TESTFILE1); /* Corrected test file name */

        file = H5Fopen(testfile, H5F_ACC_RDONLY, H5P_DEFAULT);
        if (file >= 0) {
            if (H5Oget_info_by_name3(file, "/Dataset1", &oi1, H5O_INFO_TIME, H5P_DEFAULT) < 0)
                TEST_ERROR;
            if (oi1.ctime != MTIME1) {
                H5_FAILED();
                /* If this fails, examine H5Omtime.c.  Modification time is very
                 * system dependent (e.g., on Windows DST must be hardcoded). */
                HDputs("    Old modification time incorrect");
                goto error;
            }
            if (H5Fclose(file) < 0)
                TEST_ERROR;
        }
        else {
            H5_FAILED();
            HDprintf("***cannot open the pre-created old modification test file (%s)\n", testfile);
            goto error;
        } /* end else */
    }
    PASSED();

    /* Check opening existing file with new-style modification time information
     * and make certain that the time is correct
     */
    TESTING("accessing new modification time messages");

    {
        const char *testfile = H5_get_srcdir_filename(TESTFILE2); /* Corrected test file name */

        file = H5Fopen(testfile, H5F_ACC_RDONLY, H5P_DEFAULT);
        if (file >= 0) {
            if (H5Oget_info_by_name3(file, "/Dataset1", &oi2, H5O_INFO_TIME, H5P_DEFAULT) < 0)
                TEST_ERROR;
            if (oi2.ctime != MTIME2) {
                H5_FAILED();
                HDputs("    Modification time incorrect.");
                goto error;
            }
            if (H5Fclose(file) < 0)
                TEST_ERROR;
        }
        else {
            H5_FAILED();
            HDprintf("***cannot open the pre-created old modification test file (%s)\n", testfile);
            goto error;
        } /* end else */
    }
    PASSED();

    /* Verify symbol table messages are cached */
    if (h5_verify_cached_stabs(FILENAME, fapl) < 0)
        TEST_ERROR

    /* All looks good */
    HDputs("All modification time tests passed.");
    h5_cleanup(FILENAME, fapl);
    return EXIT_SUCCESS;

    /* Something broke */
error:
    return EXIT_FAILURE;
} /* end main() */
