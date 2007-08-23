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
 * Programmer:  Quincey Koziol <koziol@ncsa.uiuc.edu>
 *              Oct 24, 2005
 *
 * Purpose:     This program is run to generate an HDF5 data file with an
 *              empty "symbol table" group.
 *
 *              This file is used in the v1.7 branch (after the "compact group"
 *              checkin) to test compatability.  Compile and run this
 *              program (with the 1.6.x branch), it will generate a file*
 *              called "group_old.h5".  You need to move it to the test
 *              directory in the HDF5 v1.7 source tree.  The test/stab.c
 *              program will read it.
 *
 */

#include "hdf5.h"

#define FILENAME "group_old.h5"

int main(void)
{
    hid_t fid = -1;             /* File ID */
    hid_t gid = -1;             /* Group creation property list ID */

    /* Create file for test groups */
    if((fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) <0) goto error;

    /* Create empty group that uses "symbol table" form to store links */
    if((gid = H5Gcreate2(fid, "old", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) goto error;
    if(H5Gclose(gid) < 0) goto error;

    /* Close file */
    if(H5Fclose(fid) < 0) goto error;

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Dclose(gid);
        H5Fclose(fid);
    } H5E_END_TRY;
    return 1;
}

