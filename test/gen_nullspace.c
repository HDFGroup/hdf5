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
 *              Saturday, April 17, 2004
 *
 * Purpose:	Create a dataset with a null dataspace and an attribute
 *      with a null dataspace.
 *		This program is used to create the test file `tnullspace.h5' which
 *      has dataspaces stored in the newer (version 2) style in the object headers.
 *		To build the test file, this program MUST be compiled and linked with
 *      the hdf5-1.7+ series of libraries and the generated test file must be
 *      put into the 'test' directory in the 1.6.x branch of the library.
 */

#include "hdf5.h"
#include <assert.h>

#define NULLFILE   "tnullspace.h5"
#define NULLDATASET  "null_dataset"
#define NULLATTR   "null_attribute"

int
main(void)
{
    hid_t fid;          /* File ID */
    hid_t gid;          /* Group ID */
    hid_t sid;          /* Dataspace ID */
    hid_t did;          /* Dataset ID */
    hid_t attr;         /* Attribute ID */
    herr_t ret;         /* Generic return value */

    /* Create the file */
    fid = H5Fcreate(NULLFILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    assert(fid>0);

    sid = H5Screate(H5S_NULL);
    assert(sid>0);

    /* Create dataset */
    did = H5Dcreate2(fid, NULLDATASET, H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    assert(did>0);

    /* Close the dataset */
    ret = H5Dclose(did);
    assert(ret>=0);

    /* Open the root group */
    gid = H5Gopen2(fid, "/", H5P_DEFAULT);
    assert(gid > 0);

    /* Create an attribute for the group */
    attr = H5Acreate2(gid, NULLATTR, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT);
    assert(attr > 0);

    /* Close attribute */
    ret = H5Aclose(attr);
    assert(ret>=0);

    /* Close the group */
    ret = H5Gclose(gid);
    assert(ret>=0);

    /* Close the dataspace */
    ret = H5Sclose(sid);
    assert(ret>=0);

    /* Close the file */
    ret = H5Fclose(fid);
    assert(ret>=0);

    return 0;
}


