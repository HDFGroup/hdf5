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

/* This test is to be built and run with the HDF5 version 1.8 */

/*********************************************************************
 * Purpose: Create testfile to be accessed and modified with the trunk.
 *
 * Developer:    Mohamad Chaarawi
 *               December 2014
 *
 *********************************************************************/

#include "hdf5.h"

#define TESTFILE "tfile_v18.h5"
#define TESTFILE_MULTI "multifile_v18.h5"


static int create_file (hid_t fapl, const char* filename);
static int access_file (hid_t fapl, const char* filename);

int main(void)
{
    hid_t fapl;

    printf("creating a file with the 1.8 branch\n");
    /* create file with sec2 driver */
    if(create_file(H5P_DEFAULT, TESTFILE) < 0) {
        fprintf(stderr, "Failed to create %s\n", TESTFILE);
        return -1;
    }

    if(access_file(H5P_DEFAULT, TESTFILE) < 0)
        return -1;

    /* Do the same as above but with the multi-vfd */
    /* create a multi file driver */
    if ((fapl=H5Pcreate(H5P_FILE_ACCESS))<0)
        return -1;
    if (H5Pset_fapl_multi(fapl, NULL, NULL, NULL, NULL, 1) < 0)
        return -1;

    printf("creating a MULTI file with the 1.8 branch\n");
    if(create_file(fapl, TESTFILE_MULTI) < 0)
        return -1;
    if(access_file(fapl, TESTFILE_MULTI) < 0)
        return -1;

    if(H5Pclose(fapl) < 0)
        return -1;

    /* Return */
    return 0;
} /* main */

static int create_file (hid_t fapl, const char* filename)
{
    /* Variables */
    hid_t fid,sid,did1,did2,did3 = -1;         /* Object Descriptors */

    /* Create a file that avoids truncation */
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) {
        fprintf(stderr, "Failed H5Fcreate %s\n", filename);
        return -1;
    }

    /* Create dataspace for dataset */
    if ((sid = H5Screate(H5S_SCALAR)) < 0) {
        fprintf(stderr, "Failed H5Screate\n");
        return -1;
    }

    /* Create dataset */
    if ((did1 = H5Dcreate2(fid, "Dataset1", H5T_NATIVE_INT, sid, H5P_DEFAULT,
                          H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        fprintf(stderr, "Failed to create Dataset1 in %s\n", filename);
        return -1;
    }

    /* Create dataset */
    if ((did2 = H5Dcreate2(fid, "Dataset2", H5T_NATIVE_INT, sid, H5P_DEFAULT,
                          H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        fprintf(stderr, "Failed to create Dataset2 in %s\n", filename);
        return -1;
    }

    /* Create dataset */
    if ((did3 = H5Dcreate2(fid, "Dataset3", H5T_NATIVE_INT, sid, H5P_DEFAULT,
                          H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        fprintf(stderr, "Failed to create Dataset3 in %s\n", filename);
        return -1;
    }

    /* Close the file, dataspace, and dataset */
    H5Sclose(sid);
    H5Dclose(did1);
    H5Dclose(did2);
    H5Dclose(did3);
    H5Fclose(fid);

    return 0;
}

static int access_file (hid_t fapl, const char* filename)
{
    hid_t fid;

    /* Re-open file */
    if ((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0) {
        fprintf(stderr, "Failed H5Fopen %s\n", filename);
        return -1;
    }

    /* Unlink the dataset */
    if (H5Ldelete(fid, "Dataset3", H5P_DEFAULT) < 0) {
        fprintf(stderr, "Failed to delete Dataset3 in %s\n", filename);
        return -1;
    }

    /* Close file */
    if (H5Fclose(fid) < 0) {
        fprintf(stderr, "Failed H5Fclose %s\n", filename);
        return -1;
    }

    return 0;
}
