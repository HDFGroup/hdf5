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

#ifndef H5CLEAR_GENTEST_H
#define H5CLEAR_GENTEST_H

#include "hdf5.h"

int gen_cache_image_file(const char *fname);
int gen_enhance_files(bool user);

#define KB 1024U

#define CACHE_IMAGE_FILE "h5clear_mdc_image.h5"
#define DSET             "DSET"
#define DATASET          "dset"
#define NUM_ELMTS        100
#define USERBLOCK        512

#endif /* H5CLEAR_GENTEST_H */