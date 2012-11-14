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
 *              Apr 17, 2007
 *
 * Purpose:     This program is run to generate an HDF5 data file with several
 *              datasets that have "bogus" messages in their object header.
 */

#include "H5private.h"
#include "hdf5.h"
#include "H5Oprivate.h"

#ifdef H5O_ENABLE_BOGUS
#define FILENAME "tbogus.h5"
#endif /* H5O_ENABLE_BOGUS */

#ifndef TRUE
#define TRUE 1
#endif /* TRUE */
#ifndef FALSE
#define FALSE 0
#endif /* FALSE */

int main(void)
{
#ifdef H5O_ENABLE_BOGUS
    hid_t fid = -1;             /* File ID */
    hid_t sid = -1;             /* Dataspace ID */
    hid_t dcpl = -1;            /* Dataset creation property list ID */
    hid_t did = -1;             /* Dataset ID */
    uint8_t bogus_flags = 0;    /* Flags for bogus message */

    /* Create file for test datasets */
    if((fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0) goto error;

    /* Create dataspace for datasets */
    if((sid = H5Screate(H5S_SCALAR)) < 0) goto error;

    /* Create dataset creation property list */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;

    /* Add property for bogus message flags */
    if(H5Pinsert2(dcpl, H5O_BOGUS_MSG_FLAGS_NAME, H5O_BOGUS_MSG_FLAGS_SIZE, &bogus_flags, NULL, NULL, NULL, NULL, NULL, NULL) < 0) goto error;

    /* Create dataset with "bogus" message, but no message flags */
    if((did = H5Dcreate2(fid, "/Dataset1", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) goto error;
    if(H5Dclose(did) < 0) goto error;

    /* Set "fail if unknown" message flag for bogus message */
    bogus_flags = H5O_MSG_FLAG_FAIL_IF_UNKNOWN;
    if(H5Pset(dcpl, H5O_BOGUS_MSG_FLAGS_NAME, &bogus_flags) < 0) goto error;

    /* Create second dataset, with "fail if unknown" message flag */
    if((did = H5Dcreate2(fid, "/Dataset2", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) goto error;
    if(H5Dclose(did) < 0) goto error;

    /* Set "mark if unknown" message flag for bogus message */
    bogus_flags = H5O_MSG_FLAG_MARK_IF_UNKNOWN;
    if(H5Pset(dcpl, H5O_BOGUS_MSG_FLAGS_NAME, &bogus_flags) < 0) goto error;

    /* Create second dataset, with "mark if unknown" message flag */
    if((did = H5Dcreate2(fid, "/Dataset3", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) goto error;
    if(H5Dclose(did) < 0) goto error;

    /* Close dataset creation property list */
    if(H5Pclose(dcpl) < 0) goto error;

    /* Close dataspace */
    if(H5Sclose(sid) < 0) goto error;

    /* Close file */
    if(H5Fclose(fid) < 0) goto error;

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Dclose(did);
        H5Sclose(sid);
        H5Pclose(dcpl);
        H5Fclose(fid);
    } H5E_END_TRY;
#else /* H5O_ENABLE_BOGUS */
    HDputs("H5O_ENABLE_BOGUS compiler macro not defined!");
#endif /* H5O_ENABLE_BOGUS */
    return 1;
}

