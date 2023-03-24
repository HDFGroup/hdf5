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

#include "vol_object_test_parallel.h"

/*
 * The array of parallel object tests to be performed.
 */
static int (*par_object_tests[])(void) = {NULL};

int
vol_object_test_parallel(void)
{
    size_t i;
    int    nerrors;

    if (MAINPROCESS) {
        HDprintf("**********************************************\n");
        HDprintf("*                                            *\n");
        HDprintf("*         VOL Parallel Object Tests          *\n");
        HDprintf("*                                            *\n");
        HDprintf("**********************************************\n\n");
    }

    for (i = 0, nerrors = 0; i < ARRAY_LENGTH(par_object_tests); i++) {
        /* nerrors += (*par_object_tests[i])() ? 1 : 0; */

        if (MPI_SUCCESS != MPI_Barrier(MPI_COMM_WORLD)) {
            if (MAINPROCESS)
                HDprintf("    MPI_Barrier() failed!\n");
        }
    }

    if (MAINPROCESS)
        HDprintf("\n");

    return nerrors;
}
