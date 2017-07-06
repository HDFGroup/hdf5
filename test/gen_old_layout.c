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
 * Programmer:  Quincey Koziol <koziol@ncsa.uiuc.edu>
 *              Thursday, May 27, 2004
 *
 * Purpose:	Create two datasets (one for version 1 and one for version 2 of
 *      the layout message), which should have dimensions too large to
 *      represent in version 1 & 2 of the storage layout message.
 *		This program is used to create the test file `tlayouto.h5' which
 *      has truncated dimension information and can be used to verify that the
 *      library has fixed up the storage size correctly.
 *		To build the test file, this program MUST be compiled and linked
 *      with version hdf5-1.6.2 or _earlier_ libraries and the generated test
 *      file must be put into the 'test' directory in the 1.7+ (or 1.6+) branch
 *      of the library.
 */

#include "hdf5.h"

#define TESTFILE   "tlayouto.h5"
#define SPACE_RANK       2
#define SPACE_DIM0       (8*1024*1024*1024ULL)
#define SPACE_DIM1       ((256*1024*1024ULL)+1ULL)


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:
 *
 * Return:	Success:
 *
 *		Failure:
 *
 * Programmer:	Quincey Koziol
 *              Friday, January  3, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t	file, space, dset, dcpl;
    herr_t      ret;
    unsigned rank=SPACE_RANK;    /* Rank of dataspace */
    hsize_t big_dims[SPACE_RANK]={SPACE_DIM0,SPACE_DIM1};      /* Large dimensions */

    /* Create the file */
    file = H5Fcreate(TESTFILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if(file<0)
        printf("file<0!\n");

    /* Create the dataspace (for dataset) */
    space = H5Screate_simple(rank,big_dims,NULL);
    if(space<0)
        printf("space<0!\n");

    /* Create a dataset creation property list */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    if(dcpl<0)
        printf("dcpl<0!\n");

    /* Make certain that the dataset's storage doesn't get allocated :-) */
    ret = H5Pset_alloc_time(dcpl,H5D_ALLOC_TIME_LATE);
    if(ret<0)
        printf("H5Pset_alloc_time() failed!\n");

    /* Create the dataset with deferred storage allocation */
    dset = H5Dcreate2(file, "Dataset", H5T_NATIVE_INT, space, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    if(dset<0)
        printf("dset<0!\n");

    H5Dclose(dset);
    H5Sclose(space);
    H5Pclose(dcpl);
    H5Fclose(file);

    return 0;
}

