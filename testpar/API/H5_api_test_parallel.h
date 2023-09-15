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

#ifndef H5_API_TEST_PARALLEL_H
#define H5_API_TEST_PARALLEL_H

#include <mpi.h>

#include "testpar.h"

#include "H5_api_test.h"

/* Define H5VL_VERSION if not already defined */
#ifndef H5VL_VERSION
#define H5VL_VERSION 0
#endif

/* Define macro to wait forever depending on version */
#if H5VL_VERSION >= 2
#define H5_API_TEST_WAIT_FOREVER H5ES_WAIT_FOREVER
#else
#define H5_API_TEST_WAIT_FOREVER UINT64_MAX
#endif

#define PARALLEL_TEST_FILE_NAME "H5_api_test_parallel.h5"
extern char H5_api_test_parallel_filename[];

#undef TESTING
#undef TESTING_2
#undef PASSED
#undef H5_FAILED
#undef H5_WARNING
#undef SKIPPED
#undef PUTS_ERROR
#undef TEST_ERROR
#undef STACK_ERROR
#undef FAIL_STACK_ERROR
#undef FAIL_PUTS_ERROR
#undef TESTING_MULTIPART

#define TESTING(WHAT)                                                                                        \
    {                                                                                                        \
        if (MAINPROCESS) {                                                                                   \
            printf("Testing %-62s", WHAT);                                                                   \
            fflush(stdout);                                                                                  \
        }                                                                                                    \
        n_tests_run_g++;                                                                                     \
    }
#define TESTING_2(WHAT)                                                                                      \
    {                                                                                                        \
        if (MAINPROCESS) {                                                                                   \
            printf("  Testing %-60s", WHAT);                                                                 \
            fflush(stdout);                                                                                  \
        }                                                                                                    \
        n_tests_run_g++;                                                                                     \
    }
#define PASSED()                                                                                             \
    {                                                                                                        \
        if (MAINPROCESS) {                                                                                   \
            puts(" PASSED");                                                                                 \
            fflush(stdout);                                                                                  \
        }                                                                                                    \
        n_tests_passed_g++;                                                                                  \
    }
#define H5_FAILED()                                                                                          \
    {                                                                                                        \
        if (MAINPROCESS) {                                                                                   \
            puts("*FAILED*");                                                                                \
            fflush(stdout);                                                                                  \
        }                                                                                                    \
        n_tests_failed_g++;                                                                                  \
    }
#define H5_WARNING()                                                                                         \
    {                                                                                                        \
        if (MAINPROCESS) {                                                                                   \
            puts("*WARNING*");                                                                               \
            fflush(stdout);                                                                                  \
        }                                                                                                    \
    }
#define SKIPPED()                                                                                            \
    {                                                                                                        \
        if (MAINPROCESS) {                                                                                   \
            puts(" -SKIP-");                                                                                 \
            fflush(stdout);                                                                                  \
        }                                                                                                    \
        n_tests_skipped_g++;                                                                                 \
    }
#define PUTS_ERROR(s)                                                                                        \
    {                                                                                                        \
        if (MAINPROCESS) {                                                                                   \
            puts(s);                                                                                         \
            AT();                                                                                            \
        }                                                                                                    \
        goto error;                                                                                          \
    }
#define TEST_ERROR                                                                                           \
    {                                                                                                        \
        H5_FAILED();                                                                                         \
        if (MAINPROCESS) {                                                                                   \
            AT();                                                                                            \
        }                                                                                                    \
        goto error;                                                                                          \
    }
#define STACK_ERROR                                                                                          \
    {                                                                                                        \
        if (MAINPROCESS) {                                                                                   \
            H5Eprint2(H5E_DEFAULT, stdout);                                                                  \
        }                                                                                                    \
        goto error;                                                                                          \
    }
#define FAIL_STACK_ERROR                                                                                     \
    {                                                                                                        \
        H5_FAILED();                                                                                         \
        if (MAINPROCESS) {                                                                                   \
            AT();                                                                                            \
            H5Eprint2(H5E_DEFAULT, stdout);                                                                  \
        }                                                                                                    \
        goto error;                                                                                          \
    }
#define FAIL_PUTS_ERROR(s)                                                                                   \
    {                                                                                                        \
        H5_FAILED();                                                                                         \
        if (MAINPROCESS) {                                                                                   \
            AT();                                                                                            \
            puts(s);                                                                                         \
        }                                                                                                    \
        goto error;                                                                                          \
    }
#define TESTING_MULTIPART(WHAT)                                                                              \
    {                                                                                                        \
        if (MAINPROCESS) {                                                                                   \
            printf("Testing %-62s", WHAT);                                                                   \
            puts("");                                                                                        \
            fflush(stdout);                                                                                  \
        }                                                                                                    \
    }

/*
 * Macros to surround an action that will be performed non-collectively. Once the
 * operation has completed, a consensus will be formed by all ranks on whether the
 * operation failed.
 */
#define BEGIN_INDEPENDENT_OP(op_name)                                                                        \
    {                                                                                                        \
        bool ind_op_failed = false;                                                                          \
                                                                                                             \
        {

#define END_INDEPENDENT_OP(op_name)                                                                          \
    }                                                                                                        \
                                                                                                             \
    op_##op_name##_end : if (MPI_SUCCESS != MPI_Allreduce(MPI_IN_PLACE, &ind_op_failed, 1, MPI_C_BOOL,       \
                                                          MPI_LOR, MPI_COMM_WORLD))                          \
    {                                                                                                        \
        if (MAINPROCESS)                                                                                     \
            printf(                                                                                          \
                "    failed to collect consensus about whether non-collective operation was successful\n");  \
        goto error;                                                                                          \
    }                                                                                                        \
                                                                                                             \
    if (ind_op_failed) {                                                                                     \
        if (MAINPROCESS)                                                                                     \
            printf("    failure detected during non-collective operation - all other ranks will now fail "   \
                   "too\n");                                                                                 \
        goto error;                                                                                          \
    }                                                                                                        \
    }

#define INDEPENDENT_OP_ERROR(op_name)                                                                        \
    ind_op_failed = true;                                                                                    \
    goto op_##op_name##_end;

hid_t create_mpi_fapl(MPI_Comm comm, MPI_Info info, bool coll_md_read);
int   generate_random_parallel_dimensions(int space_rank, hsize_t **dims_out);

extern int mpi_size;
extern int mpi_rank;

#endif
