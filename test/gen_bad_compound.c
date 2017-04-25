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
 * Programmer:  Quincey Koziol <koziol@hdfgroup.org>
 *              April 14, 2011
 *
 * Purpose:     This program is run to generate an HDF5 data file with objects
 *              that use compound datatypes with no fields (now forbidden to
 *              be created by the library, as of v1.4.x).  It must be built/run
 *              with a copy of the 1.2.x library.
 */

#include <assert.h>
#include "hdf5.h"

#define FILENAME "bad_compound.h5"

int main()
{
    hid_t       file;
    hid_t       cmpd_dt;
    hid_t       sid;
    hid_t       did;
    hid_t       aid;
    hid_t       gid;
    hsize_t     dim = 1;
    herr_t      ret;

    /* Create compound datatype, but don't insert fields */
    cmpd_dt = H5Tcreate(H5T_COMPOUND, (size_t)8);
    assert(cmpd_dt > 0);

    /* Create File */
    file = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    assert(file > 0);

    /* Create a dataspace to use */
    sid = H5Screate_simple(1, &dim, NULL);
    assert(sid > 0);

    /* Create a dataset with the bad compound datatype */
    did = H5Dcreate(file, "dataset", cmpd_dt, sid, H5P_DEFAULT);
    assert(did > 0);

    /* Create a group */
    gid = H5Gcreate(file, "group", (size_t)0);
    assert(gid > 0);

    /* Create an attribute with the bad compound datatype */
    aid = H5Acreate(gid, "attr", cmpd_dt, sid, H5P_DEFAULT);
    assert(aid > 0);

    /* Commit the datatype */ 
    ret = H5Tcommit(file, "cmpnd", cmpd_dt);
    assert(ret >= 0);

    /* Close IDs */
    ret = H5Gclose(gid);
    assert(ret >= 0);
    ret = H5Aclose(aid);
    assert(ret >= 0);
    ret = H5Sclose(sid);
    assert(ret >= 0);
    ret = H5Dclose(did);
    assert(ret >= 0);
    ret = H5Tclose(cmpd_dt);
    assert(ret >= 0);
    ret = H5Fclose(file);
    assert(ret >= 0);

    return(0);
}

