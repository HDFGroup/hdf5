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
 *
 * Purpose:     Test support stuff.
 */

#ifndef H5HLTEST_H
#define H5HLTEST_H

/* Get the HDF5 test header */
#include "h5test.h"

/* Include the High-Level private header */
#include "H5HLprivate2.h"

/* Macros used in HL tests */
#define HL_TESTING2(WHAT)                                                                                    \
    do {                                                                                                     \
        printf("Testing %-62s", WHAT);                                                                       \
        fflush(stdout);                                                                                      \
    } while (0)
#define HL_TESTING3(WHAT)                                                                                    \
    do {                                                                                                     \
        printf("Testing %-62s", WHAT);                                                                       \
        fflush(stdout);                                                                                      \
    } while (0)

/* Implements verbose 'assert' with 'goto error' exit  */
#define VERIFY(condition, string)                                                                            \
    do {                                                                                                     \
        if (!(condition))                                                                                    \
            FAIL_PUTS_ERROR(string);                                                                         \
    } while (0)

int test_packet_table_with_varlen(void);

#endif /* H5HLTEST_H */
