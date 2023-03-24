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

#ifndef VOL_TEST_H
#define VOL_TEST_H

#include <hdf5.h>
#include <H5private.h>

#include "h5vl_test_config.h"
#include "vol_test_util.h"
#include "vol_tests_disabled.h"

/* Define H5VL_VERSION if not already defined */
#ifndef H5VL_VERSION
#define H5VL_VERSION 0
#endif

/* Define macro to wait forever depending on version */
#if H5VL_VERSION >= 2
#define VOL_TEST_WAIT_FOREVER H5ES_WAIT_FOREVER
#else
#define VOL_TEST_WAIT_FOREVER UINT64_MAX
#endif

/* Moved from h5test */

/*
 * This contains the filename prefix specificied as command line option for
 * the parallel test files.
 */
H5TEST_DLLVAR char *paraprefix;
#ifdef H5_HAVE_PARALLEL
H5TEST_DLLVAR MPI_Info h5_io_info_g; /* MPI INFO object for IO */
#endif

/*
 * Print the current location on the standard output stream.
 */
#define AT() printf("   at %s:%d in %s()...\n", __FILE__, __LINE__, __func__);

/*
 * The name of the test is printed by saying TESTING("something") which will
 * result in the string `Testing something' being flushed to standard output.
 * If a test passes, fails, or is skipped then the PASSED(), H5_FAILED(), or
 * SKIPPED() macro should be called.  After H5_FAILED() or SKIPPED() the caller
 * should print additional information to stdout indented by at least four
 * spaces.  If the h5_errors() is used for automatic error handling then
 * the H5_FAILED() macro is invoked automatically when an API function fails.
 */
#define TESTING(WHAT)                                                                                        \
    {                                                                                                        \
        printf("Testing %-62s", WHAT);                                                                       \
        n_tests_run_g++;                                                                                     \
        fflush(stdout);                                                                                      \
    }
#define TESTING_2(WHAT)                                                                                      \
    {                                                                                                        \
        printf("  Testing %-60s", WHAT);                                                                     \
        n_tests_run_g++;                                                                                     \
        fflush(stdout);                                                                                      \
    }
#define PASSED()                                                                                             \
    {                                                                                                        \
        puts(" PASSED");                                                                                     \
        n_tests_passed_g++;                                                                                  \
        fflush(stdout);                                                                                      \
    }
#define H5_FAILED()                                                                                          \
    {                                                                                                        \
        puts("*FAILED*");                                                                                    \
        n_tests_failed_g++;                                                                                  \
        fflush(stdout);                                                                                      \
    }
#define H5_WARNING()                                                                                         \
    {                                                                                                        \
        puts("*WARNING*");                                                                                   \
        fflush(stdout);                                                                                      \
    }
#define SKIPPED()                                                                                            \
    {                                                                                                        \
        puts(" -SKIP-");                                                                                     \
        n_tests_skipped_g++;                                                                                 \
        fflush(stdout);                                                                                      \
    }
#define PUTS_ERROR(s)                                                                                        \
    {                                                                                                        \
        puts(s);                                                                                             \
        AT();                                                                                                \
        goto error;                                                                                          \
    }
#define TEST_ERROR                                                                                           \
    {                                                                                                        \
        H5_FAILED();                                                                                         \
        AT();                                                                                                \
        goto error;                                                                                          \
    }
#define STACK_ERROR                                                                                          \
    {                                                                                                        \
        H5Eprint2(H5E_DEFAULT, stdout);                                                                      \
        goto error;                                                                                          \
    }
#define FAIL_STACK_ERROR                                                                                     \
    {                                                                                                        \
        H5_FAILED();                                                                                         \
        AT();                                                                                                \
        H5Eprint2(H5E_DEFAULT, stdout);                                                                      \
        goto error;                                                                                          \
    }
#define FAIL_PUTS_ERROR(s)                                                                                   \
    {                                                                                                        \
        H5_FAILED();                                                                                         \
        AT();                                                                                                \
        puts(s);                                                                                             \
        goto error;                                                                                          \
    }

/*
 * Macros used for multipart tests
 */
#define TESTING_MULTIPART(WHAT)                                                                              \
    {                                                                                                        \
        printf("Testing %-62s", WHAT);                                                                       \
        HDputs("");                                                                                          \
        fflush(stdout);                                                                                      \
    }

/*
 * Begin and end an entire section of multipart tests. By placing all the
 * parts of a test between these macros, skipping to the 'error' cleanup
 * section of a test is deferred until all parts have finished.
 */
#define BEGIN_MULTIPART                                                                                      \
    {                                                                                                        \
        int nerrors = 0;

#define END_MULTIPART                                                                                        \
    if (nerrors > 0)                                                                                         \
        goto error;                                                                                          \
    }

/*
 * Begin, end and handle errors within a single part of a multipart test.
 * The PART_END macro creates a goto label based on the given "part name".
 * When a failure occurs in the current part, the PART_ERROR macro uses
 * this label to skip to the next part of the multipart test. The PART_ERROR
 * macro also increments the error count so that the END_MULTIPART macro
 * knows to skip to the test's 'error' label once all test parts have finished.
 */
#define PART_BEGIN(part_name) {
#define PART_END(part_name)                                                                                  \
    }                                                                                                        \
    part_##part_name##_end:
#define PART_ERROR(part_name)                                                                                \
    {                                                                                                        \
        nerrors++;                                                                                           \
        goto part_##part_name##_end;                                                                         \
    }
#define PART_EMPTY(part_name)                                                                                \
    {                                                                                                        \
        goto part_##part_name##_end;                                                                         \
    }
#define PART_TEST_ERROR(part_name)                                                                           \
    {                                                                                                        \
        H5_FAILED();                                                                                         \
        AT();                                                                                                \
        nerrors++;                                                                                           \
        goto part_##part_name##_end;                                                                         \
    }

/*
 * Alarm definitions to wait up (terminate) a test that runs too long.
 */
#define H5_ALARM_SEC 1200 /* default is 20 minutes */
#define ALARM_ON     TestAlarmOn()
#define ALARM_OFF    HDalarm(0)

/******************************************************************************/

/* The name of the file that all of the tests will operate on */
#define TEST_FILE_NAME "vol_test.h5"
extern char vol_test_filename[];

/* The names of a set of container groups which hold objects
 * created by each of the different types of tests.
 */
#define GROUP_TEST_GROUP_NAME         "group_tests"
#define ATTRIBUTE_TEST_GROUP_NAME     "attribute_tests"
#define DATASET_TEST_GROUP_NAME       "dataset_tests"
#define DATATYPE_TEST_GROUP_NAME      "datatype_tests"
#define LINK_TEST_GROUP_NAME          "link_tests"
#define OBJECT_TEST_GROUP_NAME        "object_tests"
#define MISCELLANEOUS_TEST_GROUP_NAME "miscellaneous_tests"

#define ARRAY_LENGTH(array) sizeof(array) / sizeof(array[0])

#define UNUSED(o) (void)(o);

#define VOL_TEST_FILENAME_MAX_LENGTH 1024

/* The maximum size of a dimension in an HDF5 dataspace as allowed
 * for this testing suite so as not to try to create too large
 * of a dataspace/datatype. */
#define MAX_DIM_SIZE 16

/*
 * XXX: Set of compatibility macros that should be replaced once the
 * VOL connector feature support situation is resolved.
 */
#define GROUP_CREATION_IS_SUPPORTED

/*
 * Global variables to keep track of statistics on the
 * number of tests skipped, failed and run total.
 */
extern size_t n_tests_run_g;
extern size_t n_tests_passed_g;
extern size_t n_tests_failed_g;
extern size_t n_tests_skipped_g;

extern uint64_t vol_cap_flags;
#endif
