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

#ifndef VOL_ASYNC_TEST_PARALLEL_H_
#define VOL_ASYNC_TEST_PARALLEL_H_

#include "vol_test_parallel.h"

int vol_async_test_parallel(void);

/********************************************************
 *                                                      *
 *      VOL connector parallel async test defines       *
 *                                                      *
 ********************************************************/

#define PAR_ASYNC_VOL_TEST_FILE        "async_vol_test_parallel.h5"
#define PAR_ASYNC_VOL_TEST_FILE_PRINTF "async_vol_test_parallel_%d.h5"

#endif /* VOL_ASYNC_TEST_PARALLEL_H_ */
