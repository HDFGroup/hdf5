/*
 * Copyright (C) 1998 NCSA
 *                    All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Wednesday, April  8, 1998
 */
#include <assert.h>
#include <hdf5.h>


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Creates a *big* dataset.
 *
 * Return:	Success:	
 *
 *		Failure:	
 *
 * Programmer:	Robb Matzke
 *              Wednesday, April  8, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main (void)
{
    hsize_t	size1[4] = {8, 1024, 1024, 1024};
    hsize_t	size2[1] = {8589934592LL};
    hid_t	plist, file, space1, space2, dset;

    /*
     * Make sure that `hsize_t' is large enough to represent the entire data
     * space.
     */
    assert (sizeof(hsize_t)>4);

    /*
     * We might be on a machine that has 32-bit files, so create an HDF5 file
     * which is a family of files.  Each member of the family will be 1GB
     */
    plist = H5Pcreate (H5P_FILE_ACCESS);
    H5Pset_family (plist, 30, H5P_DEFAULT);
    file = H5Fcreate ("big%05d.h5", H5F_ACC_TRUNC, H5P_DEFAULT, plist);

    /* Create simple data spaces according to the size specified above. */
    space1 = H5Screate_simple (4, size1, size1);
    space2 = H5Screate_simple (1, size2, size2);

    /* Create the datasets */
    dset = H5Dcreate (file, "d1", H5T_NATIVE_INT, space1, H5P_DEFAULT);
    H5Dclose (dset);
    dset = H5Dcreate (file, "d2", H5T_NATIVE_INT, space2, H5P_DEFAULT);
    H5Dclose (dset);
    
    H5Sclose (space1);
    H5Sclose (space2);
    H5Fclose (file);
    exit (0);
}
