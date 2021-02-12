/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "hdf5.h"
#include "hdf5_hl.h"

#define RANK 2

int
main(void)
{
    hid_t   file_id;
    hsize_t dims[RANK] = {2, 3};
    int     data[6]    = {1, 2, 3, 4, 5, 6};

    /* create a HDF5 file */
    file_id = H5Fcreate("ex_lite1.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* create and write an integer type dataset named "dset" */
    H5LTmake_dataset(file_id, "/dset", RANK, dims, H5T_NATIVE_INT, data);

    /* close file */
    H5Fclose(file_id);

    return 0;
}
