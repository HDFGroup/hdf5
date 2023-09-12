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

#include "H5_api_misc_test_parallel.h"

/*
 * The array of parallel miscellaneous tests to be performed.
 */
static int (*par_misc_tests[])(void) = {NULL};

int
H5_api_misc_test_parallel(void)
{
    size_t i;
    int    nerrors;

    if (MAINPROCESS) {
        printf("**********************************************\n");
        printf("*                                            *\n");
        printf("*      API Parallel Miscellaneous Tests      *\n");
        printf("*                                            *\n");
        printf("**********************************************\n\n");
    }

    for (i = 0, nerrors = 0; i < ARRAY_LENGTH(par_misc_tests); i++) {
        /* nerrors += (*par_misc_tests[i])() ? 1 : 0; */

        if (MPI_SUCCESS != MPI_Barrier(MPI_COMM_WORLD)) {
            if (MAINPROCESS)
                printf("    MPI_Barrier() failed!\n");
        }
    }

    if (MAINPROCESS)
        printf("\n");

    return nerrors;
}
