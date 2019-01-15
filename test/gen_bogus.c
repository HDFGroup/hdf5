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
 *              Apr 17, 2007
 *
 * Purpose:     This program is run to generate an HDF5 data file with several
 *              datasets that have "bogus" messages in their object header.
 */

#include "hdf5.h"
#include "H5private.h"
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

#ifdef H5O_ENABLE_BOGUS

/*
 * Create datasets in the location (in "/" or "/group") with 
 *   message id: (a) H5O_BOGUS_VALID_ID or (b)H5O_BOGUS_INVALID_ID
 *   and various unknown message flags
 */
static int
generate_datasets(hid_t loc_id, unsigned bogus_id) 
{
    hid_t sid = -1;             /* Dataspace ID */
    hid_t dcpl = -1;            /* Dataset creation property list ID */
    hid_t did = -1;             /* Dataset ID */
    uint8_t bogus_flags = 0;    /* Flags for bogus message */

    /* Create dataspace for datasets */
    if((sid = H5Screate(H5S_SCALAR)) < 0) goto error;

    /* Create dataset creation property list */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;

    /* Add property for bogus message flags */
    if(H5Pinsert2(dcpl, H5O_BOGUS_MSG_FLAGS_NAME, H5O_BOGUS_MSG_FLAGS_SIZE, &bogus_flags, NULL, NULL, NULL, NULL, NULL, NULL) < 0) goto error;

    /* Add property for bogus message ID */
    if(H5Pinsert2(dcpl, H5O_BOGUS_MSG_ID_NAME, H5O_BOGUS_MSG_ID_SIZE, &bogus_id, NULL, NULL, NULL, NULL, NULL, NULL) < 0) goto error;

    /* Create dataset with "bogus" message, but no message flags */
    if((did = H5Dcreate2(loc_id, "Dataset1", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) goto error;
    if(H5Dclose(did) < 0) goto error;

    /* Set "fail if unknown and open for write" message flag for bogus message */
    bogus_flags = H5O_MSG_FLAG_FAIL_IF_UNKNOWN_AND_OPEN_FOR_WRITE;
    if(H5Pset(dcpl, H5O_BOGUS_MSG_FLAGS_NAME, &bogus_flags) < 0) goto error;

    /* Create second dataset, with "fail if unknown" message flag */
    if((did = H5Dcreate2(loc_id, "Dataset2", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) goto error;
    if(H5Dclose(did) < 0) goto error;

    /* Set "fail if unknown always" message flag for bogus message */
    bogus_flags = H5O_MSG_FLAG_FAIL_IF_UNKNOWN_ALWAYS;
    if(H5Pset(dcpl, H5O_BOGUS_MSG_FLAGS_NAME, &bogus_flags) < 0) goto error;

    /* Create third dataset, with "fail if unknown always" message flag */
    if((did = H5Dcreate2(loc_id, "Dataset3", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) goto error;
    if(H5Dclose(did) < 0) goto error;

    /* Set "mark if unknown" message flag for bogus message */
    bogus_flags = H5O_MSG_FLAG_MARK_IF_UNKNOWN;
    if(H5Pset(dcpl, H5O_BOGUS_MSG_FLAGS_NAME, &bogus_flags) < 0) goto error;

    /* Create fourth dataset, with "mark if unknown" message flag */
    if((did = H5Dcreate2(loc_id, "Dataset4", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) goto error;
    if(H5Dclose(did) < 0) goto error;

    /* Set "shareable" message flag for bogus message */
    bogus_flags = H5O_MSG_FLAG_SHAREABLE;
    if(H5Pset(dcpl, H5O_BOGUS_MSG_FLAGS_NAME, &bogus_flags) < 0) goto error;

    /* Create fourth dataset, with "shareable" message flag */
    if((did = H5Dcreate2(loc_id, "Dataset5", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) goto error;
    if(H5Dclose(did) < 0) goto error;

    /* Close dataset creation property list */
    if(H5Pclose(dcpl) < 0) goto error;

    /* Close dataspace */
    if(H5Sclose(sid) < 0) goto error;

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Dclose(did);
        H5Sclose(sid);
        H5Pclose(dcpl);
    } H5E_END_TRY;

    return -1;
} /* generate_datasets() */
#endif

int main(void)
{
#ifdef H5O_ENABLE_BOGUS
    hid_t fid = -1;             /* File ID */
    hid_t gid = -1;             /* Group ID */

    /* Create file for test datasets */
    if((fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0) goto error;

    /* Create datasets in "/" group with bogus message H5O_BOGUS_VALID_ID */
    if(generate_datasets(fid, H5O_BOGUS_VALID_ID) < 0)
        goto error;

    if((gid = H5Gcreate2(fid, "group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;

    /* Create datasets in "/group" with bogus message H5O_BOGUS_INVALID_ID */
    if(generate_datasets(gid, H5O_BOGUS_INVALID_ID) < 0)
        goto error;

    /* Close the group */
    if(H5Gclose(gid) < 0) goto error;

    /* Close file */
    if(H5Fclose(fid) < 0) goto error;

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Gclose(gid);
        H5Fclose(fid);
    } H5E_END_TRY;
#else /* H5O_ENABLE_BOGUS */
    HDputs("H5O_ENABLE_BOGUS compiler macro not defined!");
#endif /* H5O_ENABLE_BOGUS */
    return 1;
}

