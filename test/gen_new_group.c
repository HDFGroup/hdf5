/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
 * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Programmer:  Quincey Koziol <koziol@ncsa.uiuc.edu>
 *              Oct 24, 2005
 *
 * Purpose:     This program is run to generate an HDF5 data file with both
 *              empty and compact groups.
 *
 *              To test compatibility between v1.6 and v1.7, compile and run
 *              this program, it will generate a file called "group_new.h5".
 *              You need to move it to the test directory in the HDF5 v1.6
 *              source tree.  The test/stab.c program will read it.
 *
 */

#include "hdf5.h"

#define FILENAME "group_new.h5"

int main(void)
{
#ifdef H5_GROUP_REVISION
    hid_t fid = -1;             /* File ID */
    hid_t fcpl = -1;            /* File creation property list ID */
    hid_t gid = -1;             /* Group creation property list ID */
    hid_t sid = -1;             /* Dataspace ID */
    hid_t did = -1;             /* Dataset ID */

    /* Create file creation property list */
    if((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0) goto error;

    /* Adjust group creation parameters for root group */
    /* (So that it is created in symbol-table form) */
    if(H5Pset_link_phase_change(fcpl, 0, 0) < 0) goto error;

    /* Create file for test groups */
    if((fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, H5P_DEFAULT)) <0) goto error;

    /* Close file creation property list */
    if(H5Pclose(fcpl) < 0) goto error;

    /* Create dataspace for datasets */
    if((sid = H5Screate(H5S_SCALAR)) < 0) goto error;

    /* Create empty group (w/default group creation properties) */
    if((gid = H5Gcreate(fid, "empty", (size_t)0)) < 0) goto error;
    if(H5Gclose(gid) < 0) goto error;

    /* Create group which will contain link messages (w/default group creation properties) */
    if((gid = H5Gcreate(fid, "links", (size_t)0)) < 0) goto error;

    /* Create dataset in group */
    if((did = H5Dcreate(gid, "dset1", H5T_NATIVE_INT, sid, H5P_DEFAULT)) < 0) goto error;
    if(H5Dclose(did)<0) goto error;

    /* Create second dataset in group */
    if((did = H5Dcreate(gid, "dset2", H5T_NATIVE_INT, sid, H5P_DEFAULT)) < 0) goto error;
    if(H5Dclose(did)<0) goto error;

    /* Close dataspace */
    if(H5Sclose(sid) < 0) goto error;

    /* Close group */
    if(H5Gclose(gid) < 0) goto error;

    /* Close file */
    if(H5Fclose(fid) < 0) goto error;
#endif /* H5_GROUP_REVISION */

    return 0;

#ifdef H5_GROUP_REVISION
error:
    H5E_BEGIN_TRY {
        H5Pclose(did);
        H5Sclose(sid);
        H5Dclose(gid);
        H5Dclose(fcpl);
        H5Fclose(fid);
    } H5E_END_TRY;
    return 1;
#endif /* H5_GROUP_REVISION */
}

