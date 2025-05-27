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

#ifndef H5FC_GENTEST_H
#define H5FC_GENTEST_H

#include "hdf5.h"

#define NON_V3_FILE    "h5fc_non_v3.h5"
#define EDGE_V3_FILE   "h5fc_edge_v3.h5"
#define ERR_LEVEL_FILE "h5fc_err_level.h5"

void gen_non(const char *fname);
void gen_edge(const char *fname);
void gen_err_level(const char *fname);
void gen_ext(const char *fname, unsigned new_format, unsigned what);

const char *H5FC_FILENAME[] = {"h5fc_ext1_i.h5",   /* 0 */
                               "h5fc_ext1_s.h5",   /* 1 */
                               "h5fc_ext1_f.h5",   /* 2 */
                               "h5fc_ext2_is.h5",  /* 3 */
                               "h5fc_ext2_if.h5",  /* 4 */
                               "h5fc_ext2_sf.h5",  /* 5 */
                               "h5fc_ext3_isf.h5", /* 6 */
                               "h5fc_ext_none.h5", /* 7 */
                               NULL};

#endif /* H5FC_GENTEST_H */