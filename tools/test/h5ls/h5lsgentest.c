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

#include "hdf5.h"
#include "H5private.h"
#include "h5lsgentest.h"

#define TDSET_FILENAME "tdset2.h5"

herr_t
gent_tdset(void)
{
    hid_t   file_id, dataset1_id, dataset2_id;
    hid_t   dataspace1_id, dataspace2_id;
    hid_t   datatype1_id, datatype2_id;
    hid_t   plist_id;
    hsize_t dims1[2]      = {10, 20};
    hsize_t maxdims1[2]   = {H5S_UNLIMITED, 20};
    hsize_t dims2[2]      = {30, 10};
    hsize_t maxdims2[2]   = {30, H5S_UNLIMITED};
    hsize_t chunk_dims[2] = {5, 5};
    int     i, j;
    int     data1[10][20];
    double  data2[30][10];
    herr_t  status = SUCCEED;

    /* Initialize data for dataset 1 */
    for (i = 0; i < 10; i++) {
        for (j = 0; j < 20; j++) {
            data1[i][j] = j;
        }
    }

    /* Initialize data for dataset 2 */
    for (i = 0; i < 30; i++) {
        for (j = 0; j < 10; j++) {
            data2[i][j] = (double)j;
        }
    }

    /* Create a new file using default properties */
    file_id = H5Fcreate(TDSET_FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Create the data space for the first dataset */
    dataspace1_id = H5Screate_simple(2, dims1, maxdims1);

    /* Create the data space for the second dataset */
    dataspace2_id = H5Screate_simple(2, dims2, maxdims2);

    /* Create the dataset creation property list */
    plist_id = H5Pcreate(H5P_DATASET_CREATE);

    /* Set the chunk size */
    status = H5Pset_chunk(plist_id, 2, chunk_dims);

    /* Create the datatype for the first dataset (32-bit big-endian integer) */
    datatype1_id = H5Tcopy(H5T_STD_I32BE);

    /* Create the datatype for the second dataset (64-bit big-endian float) */
    datatype2_id = H5Tcopy(H5T_IEEE_F64BE);

    /* Create the first dataset */
    dataset1_id =
        H5Dcreate2(file_id, "dset1", datatype1_id, dataspace1_id, H5P_DEFAULT, plist_id, H5P_DEFAULT);

    /* Write the first dataset */
    status = H5Dwrite(dataset1_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data1);

    /* Create the second dataset */
    dataset2_id =
        H5Dcreate2(file_id, "dset2", datatype2_id, dataspace2_id, H5P_DEFAULT, plist_id, H5P_DEFAULT);

    /* Write the second dataset */
    status = H5Dwrite(dataset2_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, data2);

    /* Close the datasets */
    status = H5Dclose(dataset1_id);
    status = H5Dclose(dataset2_id);

    /* Close the datatypes */
    status = H5Tclose(datatype1_id);
    status = H5Tclose(datatype2_id);

    /* Close the dataspaces */
    status = H5Sclose(dataspace1_id);
    status = H5Sclose(dataspace2_id);

    /* Close the property list */
    status = H5Pclose(plist_id);

    /* Close the file */
    status = H5Fclose(file_id);

    return status;
}