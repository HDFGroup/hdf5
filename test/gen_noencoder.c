/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
#include "hdf5.h"

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Generates the noencoder.h5 file used to test SZIP without
 *              an encoder present.
 *
 * Return:      Success:        zero
 *
 *              Failure:        non-zero
 *
 * Programmer:  James Laird and Nat Furrer
 *              Thursday, July 1, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t file_id;
    hid_t dset_id;
    hid_t dcpl_id;
    hid_t space_id;
    hsize_t dims, maxdims;
    int fill = 0;
    int write_buf[10] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };

    file_id = H5Fcreate("noencoder.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    dims = 10;
    maxdims = H5S_UNLIMITED;
    space_id = H5Screate_simple(1, &dims, &maxdims);

    dcpl_id = H5Pcreate(H5P_DATASET_CREATE);
    H5Pset_chunk(dcpl_id, 1, &dims);
    H5Pset_szip(dcpl_id, H5_SZIP_NN_OPTION_MASK, 4);
    H5Pset_fill_value(dcpl_id, H5T_NATIVE_INT, &fill);
    H5Pset_fill_time(dcpl_id, H5D_FILL_TIME_ALLOC);

/* Create dataset noencoder_szip_dset.h5 */
    dset_id = H5Dcreate2(file_id, "noencoder_szip_dset.h5", H5T_NATIVE_INT, space_id, H5P_DEFAULT, dcpl_id, H5P_DEFAULT);

    H5Dwrite(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, write_buf);

    H5Pclose(dcpl_id);
    H5Dclose(dset_id);

    dcpl_id = H5Pcreate(H5P_DATASET_CREATE);
    H5Pset_chunk(dcpl_id, 1, &dims);
    H5Pset_szip(dcpl_id, H5_SZIP_NN_OPTION_MASK, 4);
    H5Pset_shuffle(dcpl_id);
    H5Pset_fletcher32(dcpl_id);
    H5Pset_fill_value(dcpl_id, H5T_NATIVE_INT, &fill);
    H5Pset_fill_time(dcpl_id, H5D_FILL_TIME_ALLOC);

/* Create dataset noencoder_szip_shuffle_fletcher_dset.h5 */
    dset_id = H5Dcreate2(file_id, "noencoder_szip_shuffle_fletcher_dset.h5", H5T_NATIVE_INT, space_id, H5P_DEFAULT, dcpl_id, H5P_DEFAULT);

    H5Dwrite(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, write_buf);

    H5Pclose(dcpl_id);
    H5Dclose(dset_id);
    H5Sclose(space_id);
    H5Fclose(file_id);

    return(0);
}

