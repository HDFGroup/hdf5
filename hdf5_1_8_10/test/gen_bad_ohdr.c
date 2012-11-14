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
 *              Jan  5, 2008
 *
 * Purpose:     This program is run to generate an HDF5 data file with a
 *              root group that contains the incorrect # of object header
 *              messages.  It must be built/run with a copy of the library
 *              that has been built with the "H5O_ENABLE_BAD_MESG_COUNT" macro
 *              defined.
 */

#include "hdf5.h"
#include "H5private.h"
#include "H5Oprivate.h"

#ifdef H5O_ENABLE_BAD_MESG_COUNT
#define FILENAME "tbad_msg_count.h5"
#define GROUPNAME "Group"
#define ATTRNAME1 "Attribute #1"
#define ATTRNAME2 "Attribute #2"
#endif /* H5O_ENABLE_BAD_MESG_COUNT */

#ifndef TRUE
#define TRUE 1
#endif /* TRUE */
#ifndef FALSE
#define FALSE 0
#endif /* FALSE */

int main(void)
{
#ifdef H5O_ENABLE_BAD_MESG_COUNT
    hid_t fid = -1;             /* File ID */
    hid_t gid = -1;             /* Group ID */
    hid_t sid = -1;             /* Dataspace ID */
    hid_t aid = -1;             /* Attribute ID */
    hid_t gcpl = -1;            /* Group creation property list ID */
    hbool_t store_bad_mesg_count;       /* Flag for storing a bad message count */

    /* Create test file */
    if((fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0) goto error;

    /* Create group creation property list */
    if((gcpl = H5Pcreate(H5P_GROUP_CREATE)) < 0) goto error;

    /* Add property for storing bad message count */
    store_bad_mesg_count = TRUE;
    if(H5Pinsert2(gcpl, H5O_BAD_MESG_COUNT_NAME, H5O_BAD_MESG_COUNT_SIZE, &store_bad_mesg_count, NULL, NULL, NULL, NULL, NULL, NULL) < 0) goto error;

    /* Create group with bad object header message count */
    if((gid = H5Gcreate2(fid, GROUPNAME, H5P_DEFAULT, gcpl, H5P_DEFAULT)) < 0) goto error;

    /* Close group creation property list */
    if(H5Pclose(gcpl) < 0) goto error;

    /* Create dataspace for attributes */
    if((sid = H5Screate(H5S_SCALAR)) < 0) goto error;

    /* Create attribute on root group */
    if((aid = H5Acreate2(gid, ATTRNAME1, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0) goto error;
    if(H5Aclose(aid) < 0) goto error;

    /* Create another attribute on root group */
    if((aid = H5Acreate2(gid, ATTRNAME2, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0) goto error;
    if(H5Aclose(aid) < 0) goto error;

    /* Close dataspace */
    if(H5Sclose(sid) < 0) goto error;

    /* Close group */
    if(H5Gclose(gid) < 0) goto error;

    /* Close file */
    if(H5Fclose(fid) < 0) goto error;

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Dclose(aid);
        H5Sclose(sid);
        H5Pclose(gcpl);
        H5Fclose(fid);
    } H5E_END_TRY;
#else /* H5O_ENABLE_BAD_MESG_COUNT */
    HDputs("H5O_BAD_MESG_COUNT compiler macro not defined!");
#endif /* H5O_ENABLE_BAD_MESG_COUNT */
    return 1;
}

