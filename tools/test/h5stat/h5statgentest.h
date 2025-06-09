/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the LICENSE file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#ifndef H5STAT_GENTEST_H
#define H5STAT_GENTEST_H

#include "hdf5.h"

#define NEWGRAT_FILE      "h5stat_newgrat.h5"
#define IDX_FILE          "h5stat_idx.h5"
#define THRESHOLD_FILE    "h5stat_threshold.h5"
#define ERR_REFCOUNT_FILE "h5stat_err_refcount.h5"

herr_t gen_newgrat_file(const char *fname);
herr_t gen_threshold_file(const char *fname);
herr_t gen_idx_file(const char *fname);
herr_t gen_err_refcount(const char *fname);

#endif /* H5STAT_GENTEST_H */