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
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Monday, October 26, 1998
 *
 * Purpose:	Create a dataset with a simple data space that has the
 *		maximum possible number of dimensions. This program is used
 *		to create the test file `th5s.h5' which has a data space with
 *		a rank larger than what the library can handle.  To build the
 *		test file first change the definition of H5S_MAX_RANK in
 *		H5Spublic.h, recompile everything, then run this program.
 *		Don't forget to change H5S_MAX_RANK back to its original
 *		value and recompile once the test file is created.
 */
#include "hdf5.h"


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:
 *
 * Return:	Success:
 *
 *		Failure:
 *
 * Programmer:	Robb Matzke
 *              Monday, October 26, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t	file, space, dset;
    hsize_t	cur_dim[H5S_MAX_RANK];
    int		i;

    file = H5Fcreate("th5s.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if(file < 0)
        printf("file<0!\n");
    for(i = 0; i < H5S_MAX_RANK; i++)
        cur_dim[i] = 1;
    space = H5Screate_simple(H5S_MAX_RANK, cur_dim, NULL);
    if(space < 0)
        printf("space<0!\n");
    dset = H5Dcreate2(file, "dset", H5T_NATIVE_UCHAR, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if(dset < 0)
        printf("dset<0!\n");
    H5Sclose(space);
    H5Dclose(dset);
    H5Fclose(file);

    return 0;
}
