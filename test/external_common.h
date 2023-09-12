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
 * Purpose:     Private function for external.c and external_env.c
 */
#ifndef EXTERNAL_COMMON_H
#define EXTERNAL_COMMON_H

/* Include test header files */
#include "h5test.h"

/* A similar collection of files is used for the tests that
 * perform file I/O.
 */
#define N_EXT_FILES      4
#define PART_SIZE        25
#define TOTAL_SIZE       100
#define GARBAGE_PER_FILE 10

#ifdef __cplusplus
extern "C" {
#endif

H5TEST_DLL herr_t reset_raw_data_files(bool is_env);

#ifdef __cplusplus
}
#endif

#endif /* EXTERNAL_COMMON_H */
