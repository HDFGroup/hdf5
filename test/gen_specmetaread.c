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

/*
 * Programmer:  Quincey Koziol <koziol@hdfgroup.org>
 *              Thursday, October  8, 2009
 *
 * Purpose:	Create a file with a dataset who's raw data immediately follows
 *      its object header, so that when the dataset is unlinked from its parent
 *      group, a speculative read of the object header would get the raw data
 *      into the metadata accumulator, "polluting" it.
 *		To build the test file, this program MUST be compiled and linked with
 *      the library on the trunk as of when this file is checked in.
 */

#include "hdf5.h"
#include <assert.h>

#define FILENAME        "specmetaread.h5"
#define DIM             10

int
main(void)
{
    hid_t	fid;
    hid_t	fapl;
    hid_t       did;
    hid_t 	space;
    hsize_t     dim[1] = {DIM};
    unsigned    data[DIM];
    unsigned    u;
    herr_t      ret;         /* Generic return value */

    /* Initialize the data */
    for(u = 0; u < DIM; u++)
	data[u] = u;

    /* Create a FAPL with the metadata and small data aggregators turned off */
    fapl = H5Pcreate(H5P_FILE_ACCESS);
    assert(fapl > 0);
    ret = H5Pset_meta_block_size(fapl, (hsize_t)0);
    assert(ret >= 0);
    ret = H5Pset_small_data_block_size(fapl, (hsize_t)0);
    assert(ret >= 0);

    /* Create file */
    fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    assert(fid > 0);

    /* Close FAPL */
    ret = H5Pclose(fapl);
    assert(ret >= 0);

    /* Create dataspace */
    space = H5Screate_simple(1, dim, NULL);
    assert(space > 0);

    /* Create dataset #1 */
    did = H5Dcreate2(fid, "dset1", H5T_NATIVE_UINT, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    assert(did > 0);
    ret = H5Dwrite(did, H5T_NATIVE_UINT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data);
    assert(ret >= 0);
    ret = H5Dclose(did);
    assert(ret >= 0);

    /* Create dataset #2 */
    did = H5Dcreate2(fid, "dset2", H5T_NATIVE_UINT, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    assert(did > 0);
    ret = H5Dwrite(did, H5T_NATIVE_UINT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data);
    assert(ret >= 0);
    ret = H5Dclose(did);
    assert(ret >= 0);

    /* Close dataspace */
    ret = H5Sclose(space);
    assert(ret >= 0);

    /* Close file */
    ret = H5Fclose(fid);
    assert(ret >= 0);

    return 0;
}

