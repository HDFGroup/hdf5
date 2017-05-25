/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Programmer:  Neil Fortner <nfortne2@hdfgroup.org>
 *              Thursday, July  15, 2010
 *
 * Purpose:     Creates a file with non-default sizes of lengths and addresses.
 *              This is used to make sure that the local heap code is able to
 *              handle this case correctly, even when the heap prefix and data
 *              are contiguous.
 */

#include "hdf5.h"

#define TESTFILE   "tsizeslheap.h5"


/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:
 *
 * Return:      Success:
 *
 *              Failure:
 *
 * Programmer:  Neil Fortner
 *              Thursday, July  15, 2010
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t       file, space, dset, fcpl;

    /* Create the FCPL */
    fcpl = H5Pcreate(H5P_FILE_CREATE);
    if(fcpl < 0)
        printf("fcpl < 0!\n");

    /* Set sizeof_addr and sizeof_size to be 4 */
    if(H5Pset_sizes(fcpl, 4, 4) < 0)
        printf("H5Pset_sizes < 0!\n");

    /* Create the file */
    file = H5Fcreate(TESTFILE, H5F_ACC_TRUNC, fcpl, H5P_DEFAULT);
    if(file < 0)
        printf("file < 0!\n");

    /* Create the dataspace (for dataset) */
    space = H5Screate(H5S_SCALAR);
    if(space < 0)
        printf("space < 0!\n");

    /* Create the dataset with compound array fields */
    dset = H5Dcreate2(file, "Dataset1", H5T_NATIVE_INT, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if(dset < 0)
        printf("dset < 0!\n");

    H5Dclose(dset);
    H5Sclose(space);
    H5Fclose(file);
    H5Pclose(fcpl);

    return 0;
}
