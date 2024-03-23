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
 * Purpose:    Tests hard, soft (symbolic) & external links.
 */

#define H5G_FRIEND /*suppress error about including H5Gpkg      */
#define H5G_TESTING

#include "h5test.h"
#include "H5Gpkg.h"     /* Groups                 */
#include "H5Iprivate.h" /* IDs                      */
#include "H5Lprivate.h" /* Links                                */

#define TMPDIR        "tmp_links_env/"
#define NAME_BUF_SIZE 1024

static const char *FILENAME[] = {"extlinks_env0",        /* 0: main file */
                                 "extlinks_env1",        /* 1: target file */
                                 TMPDIR "extlinks_env1", /* 2 */
                                 NULL};

static int external_link_env(hid_t fapl, bool new_format);

/*-------------------------------------------------------------------------
 * Function:    external_link_env (moved from links.c)
 *
 * Purpose:     Verify that the target file is found successfully in "tmp_links_env" directory
 *        via searching the pathnames set in the environment variable HDF5_EXT_PREFIX.
 *        1. Target link: "extlinks_env1"
 *        2. Main file: "extlinks_env0"
 *        3. Target file is created in: "tmp_links_env/extlinks_env1"
 *         4. The environment variable "HDF5_EXT_PREFIX" is set to ".:tmp_links_env"
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 *-------------------------------------------------------------------------
 */
static int
external_link_env(hid_t fapl, bool new_format)
{
    hid_t       fid    = (H5I_INVALID_HID); /* File ID */
    hid_t       gid    = (H5I_INVALID_HID); /* Group IDs */
    const char *envval = NULL;              /* Pointer to environment variable */
    char        filename1[NAME_BUF_SIZE], filename2[NAME_BUF_SIZE],
        filename3[NAME_BUF_SIZE]; /* Holders for filename */

    if (new_format)
        TESTING("external links via environment variable (w/new group format)");
    else
        TESTING("external links via environment variable");

    if ((envval = getenv("HDF5_EXT_PREFIX")) == NULL)
        envval = "nomatch";
    if (strcmp(envval, ".:tmp_links_env") != 0)
        TEST_ERROR;

    /* Set up name for main file:"extlinks_env0" */
    h5_fixname(FILENAME[0], fapl, filename1, sizeof filename1);

    /* Set up name for external linked target file: "extlinks_env1" */
    h5_fixname(FILENAME[1], fapl, filename2, sizeof filename2);

    /* Create "tmp_links_env" directory */
    if (HDmkdir(TMPDIR, (mode_t)0755) < 0 && errno != EEXIST)
        TEST_ERROR;

    /* Set up name (location) for the target file: "tmp_links_env/extlinks1" */
    h5_fixname(FILENAME[2], fapl, filename3, sizeof filename3);

    /* Create the target file in "tmp_links_env" directory */
    if ((fid = H5Fcreate(filename3, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;
    if ((gid = H5Gcreate2(fid, "A", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Closing for target file */
    if (H5Gclose(gid) < 0)
        TEST_ERROR;
    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    /* Create the main file */
    if ((fid = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;

    /* Create external link to target file */
    if (H5Lcreate_external(filename2, "/A", fid, "ext_link", H5P_DEFAULT, H5P_DEFAULT) < 0)
        TEST_ERROR;

    /* Open object through external link */
    H5E_BEGIN_TRY
    {
        gid = H5Gopen2(fid, "ext_link", H5P_DEFAULT);
    }
    H5E_END_TRY

    /* Should be able to find the target file from pathnames set via HDF5_EXT_PREFIX */
    if (gid < 0) {
        H5_FAILED();
        puts("    Should have found the file in tmp_links_env directory.");
        goto error;
    }

    /* closing for main file */
    if (H5Gclose(gid) < 0)
        TEST_ERROR;
    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Gclose(gid);
        H5Fclose(fid);
    }
    H5E_END_TRY
    return -1;
} /* end external_link_env() */

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Test external link with environment variable HDF5_EXT_PREFIX
 *
 * Return:      EXIT_SUCCESS/EXIT_FAILURE
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    const char *driver_name; /* File driver value from environment */
    hid_t       fapl;        /* File access property lists */
    int         nerrors = 0; /* Error from tests */

    /* Get the VFD to use */
    driver_name = h5_get_test_driver_name();

    /* Splitter VFD has issues with external links */
    if (!strcmp(driver_name, "splitter")) {
        puts(" -- SKIPPED for incompatible VFD --");
        exit(EXIT_SUCCESS);
    }

    h5_reset();
    fapl = h5_fileaccess();

    nerrors += external_link_env(fapl, false) < 0 ? 1 : 0;

    /* Set the "use the latest version of the format" bounds for creating objects in the file */
    if (H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        TEST_ERROR;

    nerrors += external_link_env(fapl, true) < 0 ? 1 : 0;

    /* Verify symbol table messages are cached */
    nerrors += (h5_verify_cached_stabs(FILENAME, fapl) < 0 ? 1 : 0);

    h5_cleanup(FILENAME, fapl);

    /* Results */
    if (nerrors) {
        printf("***** %d External Link (HDF5_EXT_PREFIX) test%s FAILED! *****\n", nerrors,
               1 == nerrors ? "" : "s");
        exit(EXIT_FAILURE);
    }
    printf("All external Link (HDF5_EXT_PREFIX) tests passed.\n");

    /* clean up tmp_links_env directory created by external link tests */
    HDrmdir(TMPDIR);

    exit(EXIT_SUCCESS);

error:
    puts("*** TESTS FAILED ***");
    exit(EXIT_FAILURE);
} /* end main() */
