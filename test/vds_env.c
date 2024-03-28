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
 * Purpose:     Tests datasets with virtual layout.
 */
#include "h5test.h"

static const char *FILENAME[] = {"vds_env_virt_0", "vds_env_virt_3", "vds_env_src_2", "vds_env%%_src2", NULL};

/* I/O test config flags */
#define TEST_IO_CLOSE_SRC      0x01u
#define TEST_IO_DIFFERENT_FILE 0x02u
#define TEST_IO_REOPEN_VIRT    0x04u
#define TEST_IO_NTESTS         0x08u

#define FILENAME_BUF_SIZE 1024

#define TMPDIR "tmp_vds_env/"

/*-------------------------------------------------------------------------
 * Function:    test_vds_prefix_second
 *
 * Purpose:     Set up vds link prefix via H5Pset_virtual_prefix() to be "tmp"
 *        Should be able to access the target source files in tmp directory via the prefix set
 *        by H5Pset_virtual_prefix()
 *
 * Return:      Success:        0
 *              Failure:        -1
 *-------------------------------------------------------------------------
 */
static int
test_vds_prefix_second(unsigned config, hid_t fapl)
{
    const char *srcfilenamepct_map_orig = "vds%%%%_src";
    char       *srcfilename             = NULL;
    char       *srcfilename_map         = NULL;
    char       *vfilename               = NULL;
    char       *vfilename2              = NULL;
    char       *srcfilenamepct          = NULL;
    char       *srcfilenamepct_map      = NULL;
    hid_t       srcfile[4]              = {H5I_INVALID_HID, H5I_INVALID_HID, H5I_INVALID_HID,
                        H5I_INVALID_HID}; /* Files with source dsets */
    hid_t       vfile                   = H5I_INVALID_HID;   /* File with virtual dset */
    hid_t       dcpl                    = H5I_INVALID_HID;   /* Dataset creation property list */
    hid_t       dapl                    = H5I_INVALID_HID;   /* Dataset access property list */
    hid_t       srcspace[4]             = {H5I_INVALID_HID, H5I_INVALID_HID, H5I_INVALID_HID,
                         H5I_INVALID_HID}; /* Source dataspaces */
    hid_t       vspace[4]               = {H5I_INVALID_HID, H5I_INVALID_HID, H5I_INVALID_HID,
                       H5I_INVALID_HID}; /* Virtual dset dataspaces */
    hid_t       memspace                = H5I_INVALID_HID;   /* Memory dataspace */
    hid_t       srcdset[4]              = {H5I_INVALID_HID, H5I_INVALID_HID, H5I_INVALID_HID,
                        H5I_INVALID_HID}; /* Source datasets */
    hid_t       vdset                   = H5I_INVALID_HID;   /* Virtual dataset */
    hsize_t     dims[4]                 = {10, 26, 0, 0};    /* Data space current size */
    int         buf[10][26];                                 /* Write and expected read buffer */
    int         rbuf[10][26];                                /* Read buffer */
    int         fill = -1;                                   /* Fill value */
    int         i, j;
    char        buffer[1024]; /* buffer to read vds_prefix       */

    TESTING("basic virtual dataset I/O via H5Pset_vds_prefix(): all selection with ENV prefix");

    if (NULL == (srcfilename = malloc(FILENAME_BUF_SIZE)))
        TEST_ERROR;
    if (NULL == (srcfilename_map = malloc(FILENAME_BUF_SIZE)))
        TEST_ERROR;
    if (NULL == (vfilename = malloc(FILENAME_BUF_SIZE)))
        TEST_ERROR;
    if (NULL == (vfilename2 = malloc(FILENAME_BUF_SIZE)))
        TEST_ERROR;
    if (NULL == (srcfilenamepct = malloc(FILENAME_BUF_SIZE)))
        TEST_ERROR;
    if (NULL == (srcfilenamepct_map = malloc(FILENAME_BUF_SIZE)))
        TEST_ERROR;

    h5_fixname(FILENAME[0], fapl, vfilename, FILENAME_BUF_SIZE);
    h5_fixname(FILENAME[1], fapl, vfilename2, FILENAME_BUF_SIZE);
    h5_fixname(FILENAME[2], fapl, srcfilename, FILENAME_BUF_SIZE);
    h5_fixname_printf(FILENAME[2], fapl, srcfilename_map, FILENAME_BUF_SIZE);
    h5_fixname(FILENAME[3], fapl, srcfilenamepct, FILENAME_BUF_SIZE);
    h5_fixname_printf(srcfilenamepct_map_orig, fapl, srcfilenamepct_map, FILENAME_BUF_SIZE);

    /* create tmp directory and get current working directory path */
    if (HDmkdir(TMPDIR, (mode_t)0755) < 0 && errno != EEXIST)
        TEST_ERROR;

    /* Create DCPL */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    /* Set fill value */
    if (H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fill) < 0)
        TEST_ERROR;

    /* Set prefix to a nonexistent directory, will be overwritten by environment variable */
    if ((dapl = H5Pcreate(H5P_DATASET_ACCESS)) < 0)
        TEST_ERROR;

    if (H5Pset_virtual_prefix(dapl, "someprefix") < 0)
        TEST_ERROR;
    if (H5Pget_virtual_prefix(dapl, buffer, sizeof(buffer)) < 0)
        TEST_ERROR;

    if (strcmp(buffer, "someprefix") != 0)
        FAIL_PUTS_ERROR("vds prefix not set correctly");

    /* Create source dataspace */
    if ((srcspace[0] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;

    /* Create virtual dataspace */
    if ((vspace[0] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;

    /* Select all (should not be necessary, but just to be sure) */
    if (H5Sselect_all(srcspace[0]) < 0)
        TEST_ERROR;
    if (H5Sselect_all(vspace[0]) < 0)
        TEST_ERROR;

    /* Add virtual layout mapping */
    if (H5Pset_virtual(dcpl, vspace[0], config & TEST_IO_DIFFERENT_FILE ? srcfilename_map : ".", "src_dset",
                       srcspace[0]) < 0)
        TEST_ERROR;

    /* Create virtual file */
    if ((vfile = H5Fcreate(vfilename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;

    /* Create source file if requested */
    if (config & TEST_IO_DIFFERENT_FILE) {
        if (NULL == HDgetcwd(buffer, 1024))
            TEST_ERROR;
        if (HDchdir(TMPDIR) < 0)
            TEST_ERROR;
        if ((srcfile[0] = H5Fcreate(srcfilename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            TEST_ERROR;
        if (HDchdir(buffer) < 0)
            TEST_ERROR;
    }
    else {
        srcfile[0] = vfile;
        if (H5Iinc_ref(srcfile[0]) < 0)
            TEST_ERROR;
    }

    /* Create source dataset */
    if ((srcdset[0] = H5Dcreate2(srcfile[0], "src_dset", H5T_NATIVE_INT, srcspace[0], H5P_DEFAULT,
                                 H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Create virtual dataset */
    if ((vdset = H5Dcreate2(vfile, "v_dset", H5T_NATIVE_INT, vspace[0], H5P_DEFAULT, dcpl, dapl)) < 0)
        TEST_ERROR;

    /* Populate write buffer */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            buf[i][j] = (i * (int)(sizeof(buf[0]) / sizeof(buf[0][0]))) + j;

    /* Write data directly to source dataset */
    if (H5Dwrite(srcdset[0], H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR;

    /* Close srcdset and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (H5Dclose(srcdset[0]) < 0)
            TEST_ERROR;
        srcdset[0] = -1;

        if (config & TEST_IO_DIFFERENT_FILE) {
            if (H5Fclose(srcfile[0]) < 0)
                TEST_ERROR;
            srcfile[0] = -1;
        }
    }

    /* Reopen virtual dataset and file if config option specified */
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Dclose(vdset) < 0)
            TEST_ERROR;
        vdset = -1;
        if (H5Fclose(vfile) < 0)
            TEST_ERROR;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename2, H5F_ACC_RDWR, fapl)) < 0)
            TEST_ERROR;
        if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
            TEST_ERROR;
    }

    /* Read data through virtual dataset */
    memset(rbuf[0], 0, sizeof(rbuf));
    if (H5Dread(vdset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR;

    /* Verify read data */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++) {
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            if (rbuf[i][j] != buf[i][j]) {
                TEST_ERROR;
            }
    }

    /* Adjust write buffer */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            buf[i][j] += (int)(sizeof(buf) / sizeof(buf[0][0]));

    /* Write data through virtual dataset */
    if (H5Dwrite(vdset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR;

    /* Reopen srcdset and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (config & TEST_IO_DIFFERENT_FILE) {
            if (NULL == HDgetcwd(buffer, 1024))
                TEST_ERROR;
            if (HDchdir(TMPDIR) < 0)
                TEST_ERROR;
            if ((srcfile[0] = H5Fopen(srcfilename, H5F_ACC_RDONLY, fapl)) < 0)
                TEST_ERROR;
            if (HDchdir(buffer) < 0)
                TEST_ERROR;
        }
        if ((srcdset[0] = H5Dopen2(srcfile[0], "src_dset", H5P_DEFAULT)) < 0)
            TEST_ERROR;
    }

    /* Read data directly from source dataset */
    memset(rbuf[0], 0, sizeof(rbuf));
    if (H5Dread(srcdset[0], H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR;

    /* Verify read data */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            if (rbuf[i][j] != buf[i][j])
                TEST_ERROR;

    /* Close */
    if (H5Dclose(vdset) < 0)
        TEST_ERROR;
    vdset = -1;
    if (H5Dclose(srcdset[0]) < 0)
        TEST_ERROR;
    srcdset[0] = -1;
    if (H5Fclose(srcfile[0]) < 0)
        TEST_ERROR;
    srcfile[0] = -1;
    if (H5Fclose(vfile) < 0)
        TEST_ERROR;
    vfile = -1;
    if (H5Sclose(srcspace[0]) < 0)
        TEST_ERROR;
    srcspace[0] = -1;
    if (H5Sclose(vspace[0]) < 0)
        TEST_ERROR;
    vspace[0] = -1;
    if (H5Pclose(dapl) < 0)
        TEST_ERROR;
    dapl = -1;
    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;
    dcpl = -1;

    free(srcfilenamepct_map);
    free(srcfilenamepct);
    free(vfilename2);
    free(vfilename);
    free(srcfilename_map);
    free(srcfilename);

    PASSED();
    return 0;

error:
    free(srcfilenamepct_map);
    free(srcfilenamepct);
    free(vfilename2);
    free(vfilename);
    free(srcfilename_map);
    free(srcfilename);

    H5E_BEGIN_TRY
    {
        for (i = 0; i < (int)(sizeof(srcdset) / sizeof(srcdset[0])); i++)
            H5Dclose(srcdset[i]);
        H5Dclose(vdset);
        for (i = 0; i < (int)(sizeof(srcfile) / sizeof(srcfile[0])); i++)
            H5Fclose(srcfile[i]);
        H5Fclose(vfile);
        for (i = 0; i < (int)(sizeof(srcspace) / sizeof(srcspace[0])); i++)
            H5Sclose(srcspace[i]);
        for (i = 0; i < (int)(sizeof(vspace) / sizeof(vspace[0])); i++)
            H5Sclose(vspace[i]);
        H5Sclose(memspace);
        H5Pclose(dapl);
        H5Pclose(dcpl);
    }
    H5E_END_TRY

    return 1;
} /* end test_vds_prefix2 */

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Tests datasets with virtual layout
 *
 * Return:      EXIT_SUCCESS/EXIT_FAILURE
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t        fapl, my_fapl;
    unsigned     bit_config;
    H5F_libver_t low, high;   /* Low and high bounds */
    const char  *driver_name; /* File Driver value from environment */
    bool         driver_is_parallel;
    int          nerrors = 0;

    driver_name = h5_get_test_driver_name();

    /* Testing setup */
    h5_reset();
    fapl = h5_fileaccess();

    if (h5_using_parallel_driver(fapl, &driver_is_parallel) < 0)
        TEST_ERROR;

    /*
     * Skip VDS tests for parallel-enabled and splitter VFDs. VDS currently
     * doesn't support parallel reads and the splitter VFD has external
     * link-related bugs.
     */
    if (driver_is_parallel || !strcmp(driver_name, "splitter")) {
        puts(" -- SKIPPED for incompatible VFD --");
        exit(EXIT_SUCCESS);
    }

    /* Set to use the latest file format */
    if ((my_fapl = H5Pcopy(fapl)) < 0)
        TEST_ERROR;

    /* Loop through all the combinations of low/high version bounds */
    for (low = H5F_LIBVER_EARLIEST; low < H5F_LIBVER_NBOUNDS; low++) {
        for (high = H5F_LIBVER_EARLIEST; high < H5F_LIBVER_NBOUNDS; high++) {
            char        msg[80];     /* Message for file version bounds */
            const char *low_string;  /* The low bound string */
            const char *high_string; /* The high bound string */

            /* Invalid combinations, just continue */
            if (high == H5F_LIBVER_EARLIEST || high < low)
                continue;

            /* Test virtual dataset only for V110 and above */
            if (high < H5F_LIBVER_V110)
                continue;

            /* Set the low/high version bounds */
            if (H5Pset_libver_bounds(my_fapl, low, high) < 0)
                TEST_ERROR;

            /* Display testing info */
            low_string  = h5_get_version_string(low);
            high_string = h5_get_version_string(high);
            snprintf(msg, sizeof(msg),
                     "Testing virtual dataset with file version bounds: (%s, %s):", low_string, high_string);
            puts(msg);

            for (bit_config = 0; bit_config < TEST_IO_NTESTS; bit_config++) {
                printf("Config: %s%s%s\n", bit_config & TEST_IO_CLOSE_SRC ? "closed source dataset, " : "",
                       bit_config & TEST_IO_DIFFERENT_FILE ? "different source file" : "same source file",
                       bit_config & TEST_IO_REOPEN_VIRT ? ", reopen virtual file" : "");
                nerrors += test_vds_prefix_second(bit_config, fapl);
            }

            /* Verify symbol table messages are cached */
            nerrors += (h5_verify_cached_stabs(FILENAME, my_fapl) < 0 ? 1 : 0);

        } /* end for high */
    }     /* end for low */

    if (H5Pclose(my_fapl) < 0)
        TEST_ERROR;

    if (nerrors)
        goto error;
    printf("All virtual dataset tests passed.\n");
    h5_cleanup(FILENAME, fapl);

    return EXIT_SUCCESS;

error:
    nerrors = MAX(1, nerrors);
    printf("***** %d VIRTUAL DATASET TEST%s FAILED! *****\n", nerrors, 1 == nerrors ? "" : "S");
    return EXIT_FAILURE;
} /* end main() */
