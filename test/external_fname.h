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
 * Purpose:     Private declaration for external.c and external_env.c
 */
#ifndef EXTERNAL_FNAME_H
#define EXTERNAL_FNAME_H

/* Include test header files */
#include "h5test.h"

static const char *EXT_FNAME[] = {"extern_1",          "extern_2", "extern_3", "extern_4",
                                  "extern_dir/file_1", "extern_5", NULL};

#endif /* EXTERNAL_FNAME_H */
