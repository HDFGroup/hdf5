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

#ifndef H5PERF_GENTEST_H
#define H5PERF_GENTEST_H

#include "hdf5.h"

#define FNAME     "test_perf.h5"
#define NGROUPS   20
#define NDSETS    20
#define NATTRS    20
#define DIM0      40
#define NROWS     100
#define NTYPES    9
#define MAXVLEN   10
#define FIXED_LEN 8

herr_t create_perf_test_file(const char *fname, int ngrps, int ndsets, int nattrs, hsize_t nrows,
                             hsize_t dim0, hsize_t chunk, int vlen, int compressed, int latest);

#endif /* H5PERF_GENTEST_H */