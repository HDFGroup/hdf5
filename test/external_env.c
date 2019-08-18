/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Purpose:    Tests datasets stored in external raw files.
 */
#include "external_common.h"

static const char *EXT_ENV_FNAME[] = {
    "extern_env_dir/file_2",
    NULL
};


/*-------------------------------------------------------------------------
 * Function:    test_path_env
 *
 * Purpose:     Test whether the value of HDF5_EXTFILE_PREFIX will overwrite
 *              the efile_prefix dataset access property.
 *              This will create an HDF5 file in a subdirectory which will
 *              refer to ../extern_*a.raw
 *              The files are then accessed by setting the HDF5_EXTFILE_PREFIX
 *              environment variable to "${ORIGIN}".
 *              The efile_prefix dataset access property is set to "someprefix",
 *              which will cause an error if the value is not overwritten by
 *              the environment variable.
 *
 * Return:      Success:    0
 *              Failure:    1
 *
 * Programmer:  Steffen Kiess
 *              March 10, 2015
 *
 *-------------------------------------------------------------------------
 */
static int
test_path_env(hid_t fapl)
{
    hid_t    file = -1;          /* file to write to                     */
    hid_t    dcpl = -1;          /* dataset creation properties          */
    hid_t    space = -1;         /* data space                           */
    hid_t    dapl = -1;          /* dataset access property list         */
    hid_t    dset = -1;          /* dataset                              */
    size_t    i;                  /* miscellaneous counters               */
    char    cwdpath[1024];        /* working directory                    */
    char    filename[1024];        /* file name                            */
    int        part[PART_SIZE];    /* raw data buffer (partial)            */
    int     whole[TOTAL_SIZE];  /* raw data buffer (total)              */
    hsize_t    cur_size;           /* current data space size              */
    char    buffer[1024];       /* buffer to read efile_prefix          */

    TESTING("prefix in HDF5_EXTFILE_PREFIX");

    h5_fixname(EXT_ENV_FNAME[0], fapl, filename, sizeof(filename));
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Reset the raw data files */
    if(reset_raw_data_files(TRUE) < 0)
        TEST_ERROR

    /* Create the dataset */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR
    if(NULL == HDgetcwd(cwdpath, sizeof(cwdpath)))
        TEST_ERROR
    for(i = 0; i < N_EXT_FILES; i++) {
        HDsnprintf(filename, sizeof(filename), "extern_env_%dr.raw", (int) i + 1);
        if(H5Pset_external(dcpl, filename, (off_t)(i * GARBAGE_PER_FILE), (hsize_t)sizeof(part)) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    cur_size = TOTAL_SIZE;
    if((space = H5Screate_simple(1, &cur_size, NULL)) < 0)
        FAIL_STACK_ERROR
    if((dapl = H5Pcreate(H5P_DATASET_ACCESS)) < 0)
        FAIL_STACK_ERROR

    /* Set prefix to a nonexistent directory, will be overwritten by environment variable */
    if(H5Pset_efile_prefix(dapl, "someprefix") < 0)
        FAIL_STACK_ERROR
    if(H5Pget_efile_prefix(dapl, buffer, sizeof(buffer)) < 0)
        FAIL_STACK_ERROR
    if(HDstrcmp(buffer, "someprefix") != 0)
        FAIL_PUTS_ERROR("efile prefix not set correctly");

    /* Create dataset */
    if((dset = H5Dcreate2(file, "dset1", H5T_NATIVE_INT, space, H5P_DEFAULT, dcpl, dapl)) < 0)
        FAIL_STACK_ERROR

    /* Read the entire dataset and compare with the original */
    HDmemset(whole, 0, sizeof(whole));
    if(H5Dread(dset, H5T_NATIVE_INT, space, space, H5P_DEFAULT, whole) < 0)
        FAIL_STACK_ERROR
    for(i = 0; i < TOTAL_SIZE; i++)
        if(whole[i] != (signed)i)
            FAIL_PUTS_ERROR("Incorrect value(s) read.");

    if(H5Dclose(dset) < 0) FAIL_STACK_ERROR
    if(H5Pclose(dapl) < 0) FAIL_STACK_ERROR
    if(H5Pclose(dcpl) < 0) FAIL_STACK_ERROR
    if(H5Sclose(space) < 0) FAIL_STACK_ERROR
    if(H5Fclose(file) < 0) FAIL_STACK_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dapl);
        H5Dclose(dset);
        H5Pclose(dcpl);
        H5Sclose(space);
        H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* end test_path_env() */


/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Runs external dataset tests.
 *
 * Return:      EXIT_SUCCESS/EXIT_FAILURE
 *
 * Programmer:    Robb Matzke
 *              Tuesday, March  3, 1998
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t    fapl_id_old = -1;   /* file access properties (old format)  */
    hid_t    fapl_id_new = -1;   /* file access properties (new format)  */
    hid_t    fid = -1;           /* file for test_1* functions           */
    hid_t    gid = -1;           /* group to emit diagnostics            */
    unsigned latest_format;     /* default or latest file format        */
    int        nerrors = 0;        /* number of errors                     */

    h5_reset();

    /* Get a fapl for the old (default) file format */
    fapl_id_old = h5_fileaccess();

    /* Copy and set up a fapl for the latest file format */
    if((fapl_id_new = H5Pcopy(fapl_id_old)) < 0)
        FAIL_STACK_ERROR
    if(H5Pset_libver_bounds(fapl_id_new, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        FAIL_STACK_ERROR

    if(HDmkdir("extern_env_dir", (mode_t)0755) < 0 && errno != EEXIST)
        TEST_ERROR;

    /* Test with old & new format groups */
    for(latest_format = FALSE; latest_format <= TRUE; latest_format++) {
        hid_t current_fapl_id = -1;

        /* Set the fapl for different file formats */
        if(latest_format) {
            HDputs("\nTesting with the latest file format:");
            current_fapl_id = fapl_id_new;
        } /* end if */
        else {
            HDputs("Testing with the default file format:");
            current_fapl_id = fapl_id_old;
        } /* end else */

        nerrors += test_path_env(current_fapl_id);
    } /* end for */

    if(nerrors > 0) goto error;

    /* Close the new ff fapl. h5_cleanup will take care of the old ff fapl */
    if(H5Pclose(fapl_id_new) < 0) FAIL_STACK_ERROR

    HDputs("All external storage tests passed.");

    /* Clean up files used by file set tests */
    if(h5_cleanup(EXT_ENV_FNAME, fapl_id_old)) {
        char	filename[1024];		/* file name                            */
        int     i;

        for(i = 0; i < N_EXT_FILES; i++) {
            HDsnprintf(filename, sizeof(filename), "extern_env_dir%sextern_env_%dr.raw", H5_DIR_SEPS, i + 1);
            HDremove(filename);
            HDsnprintf(filename, sizeof(filename), "extern_env_dir%sextern_env_%dw.raw", H5_DIR_SEPS, i + 1);
            HDremove(filename);
        }

        HDrmdir("extern_env_dir");
    } /* end if */

    return EXIT_SUCCESS;

error:
    H5E_BEGIN_TRY {
        H5Fclose(fid);
        H5Pclose(fapl_id_old);
        H5Pclose(fapl_id_new);
        H5Gclose(gid);
    } H5E_END_TRY;
    nerrors = MAX(1, nerrors);
    HDprintf("%d TEST%s FAILED.\n", nerrors, 1 == nerrors ? "" : "s");
    return EXIT_FAILURE;
} /* end main() */

