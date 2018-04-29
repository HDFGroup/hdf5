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
 * Purpose:	Generate an HDF5 file for testing H5FFV-10216
 */
#include "h5test.h"

#define TESTFILE    "bad_offset.h5"
#define GRP1        "group1"
#define GRP2        "group2"
#define DSET        "dsetA"
#define SOFT1       "soft_one"
#define SOFT2       "soft_two"



/*-------------------------------------------------------------------------
 * Function:	main
 *
 *              Generate an HDF5 file with groups, datasets and symbolic links. 
 *              After the file is generated, write bad offset values to 
 *              the heap at 3 locations in the file:
 *              (A) Open the file:
 *                  fd = HDopen(TESTFILE, O_RDWR, 0663);
 *              (B) Position the file at:
 *                  (1) HDlseek(fd, (HDoff_t)880, SEEK_SET);
 *                      "/group1/group2": replace heap offset "8" by bad offset
 *                  (2) HDlseek(fd, (HDoff_t)1512, SEEK_SET);
 *                      "/dsetA": replace name offset into private heap "72" by bad offset 
 *                  (3) HDlseek(fd, (HDoff_t)1616, SEEK_SET);
 *                      /soft_one: replace link value offset in the scratch pad "32" by bad offset
 *              (C) Write the bad offset value to the file for (1), (2) and (3):
 *                  write(fd, &val, sizeof(val));
 *
 *              Note: if the groups/datasets/symbolic links are changed in the file,
 *              the above locations need to be adjusted accordingly.
 *
 * Return:      EXIT_SUCCESS/EXIT_FAILURE
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t       fid = -1, gid1 = -1, gid2 = -1; /* File and group IDs */
    hid_t       did = -1, sid = -1;             /* Dataset and dataspace IDs */
    int         fd = -1;                        /* File descriptor */
    int64_t     val = 999;                      /* Bad offset value */

    /* Create the test file */
    if((fid = H5Fcreate(TESTFILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Create two groups */
    if((gid1 = H5Gcreate2(fid, GRP1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR
    if((gid2 = H5Gcreate2(gid1, GRP2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Close the groups */
    if(H5Gclose(gid1) < 0)
        FAIL_STACK_ERROR
    if(H5Gclose(gid2) < 0)
        FAIL_STACK_ERROR

    /* Create soft links to the groups */
    if(H5Lcreate_soft("/group1", fid, SOFT1, H5P_DEFAULT, H5P_DEFAULT) < 0)
        FAIL_STACK_ERROR
    if(H5Lcreate_soft("/group1/group2", fid, SOFT2, H5P_DEFAULT, H5P_DEFAULT) < 0)
        FAIL_STACK_ERROR

    /* Create a dataset */
    if((sid = H5Screate(H5S_SCALAR)) < 0)
        FAIL_STACK_ERROR
    if((did = H5Dcreate2(fid, DSET, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) <  0)
        FAIL_STACK_ERROR

    /* Close the dataset */
    if(H5Dclose(did) < 0)
        FAIL_STACK_ERROR

    /* Close the dataspace */
    if(H5Sclose(sid) < 0)
        FAIL_STACK_ERROR

    /* Close the file */
    if(H5Fclose(fid) < 0)
        FAIL_STACK_ERROR

    /* 
     * Write bad offset values at 3 locations in the file
     */

    /* Open the file */
    if((fd = HDopen(TESTFILE, O_RDWR, 0663)) < 0)
        FAIL_STACK_ERROR

    /* Position the file for /group1/group2: replace heap offset "8" by bad offset */
    if(HDlseek(fd, (HDoff_t)880, SEEK_SET) < 0)
        FAIL_STACK_ERROR
    /* Write the bad offset value to the file */
    if(HDwrite(fd, &val, sizeof(val)) < 0)
        FAIL_STACK_ERROR

    /* Position the file for /dsetA: replace name offset into private heap "72" by bad offset */
    if(HDlseek(fd, (HDoff_t)1512, SEEK_SET) < 0)
        FAIL_STACK_ERROR
    /* Write the bad offset value to the file */
    if(HDwrite(fd, &val, sizeof(val)) < 0)
        FAIL_STACK_ERROR

    /* Position the file for /soft_one: replace link value offset in the scratch pad "32" by bad offset */
    if(HDlseek(fd, (HDoff_t)1616, SEEK_SET) < 0)
        FAIL_STACK_ERROR
    /* Write the bad offset value to the file */
    if(HDwrite(fd, &val, sizeof(val)) < 0)
        FAIL_STACK_ERROR

    /* Close the file */
    if(HDclose(fd) < 0)
        FAIL_STACK_ERROR

    return EXIT_SUCCESS;

error:
    H5E_BEGIN_TRY {
        H5Gclose(gid1);
        H5Gclose(gid2);
        H5Dclose(did);
        H5Sclose(sid);
        H5Fclose(fid);
    } H5E_END_TRY;

    return EXIT_FAILURE;
} /* end main() */

