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

#ifndef VOL_TEST_UTIL_H_
#define VOL_TEST_UTIL_H_

#include "hdf5.h"

hid_t generate_random_datatype(H5T_class_t parent_class, hbool_t is_compact);
hid_t generate_random_dataspace(int rank, const hsize_t *max_dims, hsize_t *dims_out, hbool_t is_compact);
int   create_test_container(char *filename);

#endif /* VOL_TEST_UTIL_H_ */
