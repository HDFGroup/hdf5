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

#ifndef VOL_ASYNC_TEST_H
#define VOL_ASYNC_TEST_H

#include "vol_test.h"

int vol_async_test(void);

/************************************************
 *                                              *
 *      VOL connector async test defines        *
 *                                              *
 ************************************************/

#define ASYNC_VOL_TEST_FILE        "async_vol_test.h5"
#define ASYNC_VOL_TEST_FILE_PRINTF "async_vol_test_%d.h5"

#endif
