/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
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
 * HDF5 Subfiling VFD tests
 */

#include <mpi.h>
#include <libgen.h>

#include "testpar.h"
#include "H5srcdir.h"

#ifdef H5_HAVE_SUBFILING_VFD

#include "H5FDsubfiling.h"
#include "H5FDioc.h"

#define SUBFILING_TEST_DIR H5FD_SUBFILING_NAME

#ifndef PATH_MAX
#define PATH_MAX 4096
#endif

#define ARRAY_SIZE(a) sizeof(a) / sizeof(a[0])

static MPI_Comm comm = MPI_COMM_WORLD;
static MPI_Info info = MPI_INFO_NULL;
static int      mpi_rank;
static int      mpi_size;

int nerrors = 0;

/* Function pointer typedef for test functions */
typedef void (*test_func)(void);

/* Utility functions */
static herr_t build_test_filename(const char *base_filename, const char *test_dirname, hid_t fapl_id,
                                  H5FD_subfiling_config_t *subfiling_config);
static hid_t  create_subfiling_ioc_fapl(const char *base_filename);

/* Test functions */
static void test_create_and_close(void);

static test_func tests[] = {
    test_create_and_close,
};

static herr_t
build_test_filename(const char *base_filename, const char *test_dirname, hid_t fapl_id,
                    H5FD_subfiling_config_t *subfiling_config)
{
    char * path      = NULL;
    char * real_dir  = NULL;
    char * full_name = NULL;
    herr_t ret_value = SUCCEED;

    if (!base_filename || *base_filename == '\0')
        TEST_ERROR;

    if (NULL == (path = HDmalloc(H5FD_SUBFILING_PATH_MAX)))
        TEST_ERROR;
    if (NULL == (full_name = HDmalloc(H5FD_SUBFILING_PATH_MAX)))
        TEST_ERROR;

    errno = 0;

    if (NULL == (real_dir = HDrealpath(test_dirname, NULL))) {
        HDperror("HDrealpath failed");
        TEST_ERROR;
    }

    if (HDsnprintf(path, H5FD_SUBFILING_PATH_MAX, "%s%s%s", real_dir,
                   (real_dir[HDstrlen(real_dir)] == '/') ? "" : "/", /* add slash if needed */
                   base_filename) < 0)
        TEST_ERROR;

    if (!h5_fixname(path, fapl_id, full_name, H5FD_SUBFILING_PATH_MAX))
        TEST_ERROR;

    HDstrncpy(subfiling_config->file_path, full_name, H5FD_SUBFILING_PATH_MAX);
    HDstrncpy(subfiling_config->file_dir, dirname(full_name), H5FD_SUBFILING_PATH_MAX);

    HDfree(full_name);
    HDfree(real_dir);
    HDfree(path);

    return ret_value;

error:
    HDfree(full_name);
    HDfree(real_dir);
    HDfree(path);

    return FAIL;
}

/* ---------------------------------------------------------------------------
 * Function:    create_subfiling_ioc_fapl
 *
 * Purpose:     Create and populate a subfiling FAPL ID that uses either the
 *              IOC VFD or sec2 VFD.
 *
 * Return:      Success: HID of the top-level (subfiling) FAPL, a non-negative
 *                       value.
 *              Failure: H5I_INVALID_HID, a negative value.
 * ---------------------------------------------------------------------------
 */
static hid_t
create_subfiling_ioc_fapl(const char *base_filename)
{
    H5FD_subfiling_config_t *subfiling_conf = NULL;
    H5FD_ioc_config_t *      ioc_conf       = NULL;
    hid_t                    ioc_fapl       = H5I_INVALID_HID;
    hid_t                    ret_value      = H5I_INVALID_HID;

    if (base_filename == NULL || *base_filename == '\0')
        TEST_ERROR;

    if (NULL == (subfiling_conf = HDcalloc(1, sizeof(*subfiling_conf))))
        TEST_ERROR;

    if ((ioc_fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;

    if ((ret_value = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;

    if (H5Pset_mpi_params(ret_value, comm, info) < 0)
        TEST_ERROR

    /* Get defaults for Subfiling configuration */
    if (H5Pget_fapl_subfiling(ret_value, subfiling_conf) < 0)
        TEST_ERROR;

    if (subfiling_conf->require_ioc) {
        if (NULL == (ioc_conf = HDcalloc(1, sizeof(*ioc_conf))))
            TEST_ERROR;

        /* Get IOC VFD defaults */
        if (H5Pget_fapl_ioc(ioc_fapl, ioc_conf) < 0)
            TEST_ERROR;

        if (H5Pset_mpi_params(ioc_fapl, comm, info) < 0)
            TEST_ERROR

        if (H5Pset_fapl_ioc(ioc_fapl, ioc_conf) < 0)
            TEST_ERROR;
    }
    else {
        if (H5Pset_fapl_sec2(ioc_fapl) < 0)
            TEST_ERROR;
    }

    subfiling_conf->ioc_fapl_id = ioc_fapl;

    /* TODO: build_test_filename uses fapl ID for h5_fixname, but we haven't
     * set subfiling VFD yet because we can't until filenames are filled in
     */
    if (build_test_filename(base_filename, SUBFILING_TEST_DIR, ret_value, subfiling_conf) < 0)
        TEST_ERROR;

    if (H5Pset_fapl_subfiling(ret_value, subfiling_conf) < 0)
        TEST_ERROR;

    HDfree(ioc_conf);
    HDfree(subfiling_conf);

    return ret_value;

error:
    HDfree(ioc_conf);
    HDfree(subfiling_conf);

    if ((H5I_INVALID_HID != ioc_fapl) && (H5Pclose(ioc_fapl) < 0)) {
        H5_FAILED();
        AT();
    }
    if ((H5I_INVALID_HID != ret_value) && (H5Pclose(ret_value) < 0)) {
        H5_FAILED();
        AT();
    }

    return H5I_INVALID_HID;
}

/*
 * A simple test that creates and closes a file with the
 * subfiling VFD
 */
static void
test_create_and_close(void)
{
    H5FD_subfiling_config_t subfiling_config;
    hid_t                   file_id = H5I_INVALID_HID;
    hid_t                   fapl_id = H5I_INVALID_HID;

    if (MAINPROCESS)
        TESTING("File creation and immediate close");

    fapl_id = create_subfiling_ioc_fapl("basic_create");
    VRFY((fapl_id >= 0), "FAPL creation succeeded");

    VRFY((H5Pget_fapl_subfiling(fapl_id, &subfiling_config) >= 0), "H5Pget_fapl_subfiling succeeded");

    file_id = H5Fcreate(subfiling_config.file_path, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
    VRFY((file_id >= 0), "H5Fcreate succeeded");

    if (H5P_DEFAULT != subfiling_config.ioc_fapl_id)
        VRFY((H5Pclose(subfiling_config.ioc_fapl_id) >= 0), "FAPL close succeeded");
    VRFY((H5Pclose(fapl_id) >= 0), "FAPL close succeeded");
    VRFY((H5Fclose(file_id) >= 0), "File close succeeded");

    return;
}

int
main(int argc, char **argv)
{
    int required = MPI_THREAD_MULTIPLE;
    int provided = 0;
    int mpi_code;

    /* Initialize MPI */
    if (MPI_SUCCESS != MPI_Init_thread(&argc, &argv, required, &provided)) {
        HDprintf("MPI_Init_thread failed\n");
        nerrors++;
        goto exit;
    }

    if (provided != required) {
        HDprintf("MPI doesn't support MPI_Init_thread with MPI_THREAD_MULTIPLE\n");
        nerrors++;
        goto exit;
    }

    MPI_Comm_size(comm, &mpi_size);
    MPI_Comm_rank(comm, &mpi_rank);

    H5open();

    if (H5dont_atexit() < 0) {
        if (MAINPROCESS)
            HDprintf("Failed to turn off atexit processing. Continue.\n");
    }

    /* Enable selection I/O using internal temporary workaround */
    H5_use_selection_io_g = TRUE;

    if (MAINPROCESS) {
        HDprintf("Testing Subfiling VFD functionality\n");
    }

    TestAlarmOn();

    /* Create directories for test-generated .h5 files */
    if ((HDmkdir(SUBFILING_TEST_DIR, (mode_t)0755) < 0) && (errno != EEXIST)) {
        HDprintf("couldn't create subfiling testing directory\n");
        nerrors++;
        goto exit;
    }

    for (size_t i = 0; i < ARRAY_SIZE(tests); i++) {
        if (MPI_SUCCESS == (mpi_code = MPI_Barrier(comm))) {
            (*tests[i])();
        }
        else {
            if (MAINPROCESS)
                MESG("MPI_Barrier failed");
            nerrors++;
        }
    }

    if (nerrors)
        goto exit;

    if (MAINPROCESS)
        HDputs("All Subfiling VFD tests passed\n");

exit:
    if (nerrors) {
        if (MAINPROCESS)
            HDprintf("*** %d TEST ERROR%s OCCURRED ***\n", nerrors, nerrors > 1 ? "S" : "");
    }

    TestAlarmOff();

#if 0 /* TODO */
    h5_clean_files(FILENAME, fapl);
#endif

    H5close();

    MPI_Finalize();

    HDexit(nerrors ? EXIT_FAILURE : EXIT_SUCCESS);
}

#else /* H5_HAVE_SUBFILING_VFD */

int
main(void)
{
    h5_reset();
    HDprintf("Testing Subfiling VFD functionality\n");
    HDprintf("SKIPPED - Subfiling VFD not built\n");
    HDexit(EXIT_SUCCESS);
}

#endif /* H5_HAVE_SUBFILING_VFD */
