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
 * Purpose:	This program tests family files after being repartitioned
 *              by h5repart.  It simply tries to reopen the files with
 *              correct family driver and member size.
 */
#include "hdf5.h"
#include "H5private.h"

#define KB                      1024
#define FAMILY_H5REPART_SIZE1   20000
#define FAMILY_H5REPART_SIZE2   (5*KB)

const char *FILENAME[] = {
    "fst_family%05d.h5",
    "scd_family%05d.h5",
    "family_to_single.h5",
    "family_to_sec2.h5",
    NULL
};

herr_t test_family_h5repart_opens(void);
herr_t test_single_h5repart_opens(void);


/*-------------------------------------------------------------------------
 * Function:    test_family_h5repart_opens
 *
 * Purpose:     Tries to reopen family files.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
test_family_h5repart_opens(void)
{
    hid_t       fid = -1;
    hid_t       fapl_id = -1;

    /* open 1st file(single member file) with correct family size(20000 byte) */
    if ((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        goto error;

    if (H5Pset_fapl_family(fapl_id, (hsize_t)FAMILY_H5REPART_SIZE1, H5P_DEFAULT) < 0)
        goto error;

    if ((fid = H5Fopen(FILENAME[0], H5F_ACC_RDWR, fapl_id))<0)
        goto error;

    if (H5Fclose(fid) < 0)
        goto error;

    /* open 2nd file(multiple member files) with correct family size(5KB) */
    if (H5Pset_fapl_family(fapl_id, (hsize_t)FAMILY_H5REPART_SIZE2, H5P_DEFAULT) < 0)
        goto error;

    if ((fid = H5Fopen(FILENAME[1], H5F_ACC_RDWR, fapl_id)) < 0)
        goto error;

    if (H5Pclose(fapl_id) < 0)
        goto error;

    if (H5Fclose(fid) < 0)
        goto error;

    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Pclose(fapl_id);
        H5Fclose(fid);
    } H5E_END_TRY;

    return FAIL;

} /* end test_family_h5repart_opens() */



/*-------------------------------------------------------------------------
 * Function:    test_single_h5repart_opens
 *
 * Purpose:     Tries to reopen a single file.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
test_single_h5repart_opens(void)
{
    hid_t       fid = -1;

    /* open the single file */
    if ((fid = H5Fopen(FILENAME[2], H5F_ACC_RDWR, H5P_DEFAULT)) < 0)
        goto error;
    if (H5Fclose(fid) < 0)
        goto error;

    /* open the single file (created using the old argument) */
    if ((fid = H5Fopen(FILENAME[3], H5F_ACC_RDWR, H5P_DEFAULT)) < 0)
        goto error;
    if (H5Fclose(fid) < 0)
        goto error;

    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Fclose(fid);
    } H5E_END_TRY;

    return FAIL;

} /* end test_single_h5repart_opens() */


/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Tests h5repart-ed family files
 *
 * Return:      EXIT_SUCCESS/EXIT_FAILURE
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    int     nerrors = 0;

    nerrors += test_family_h5repart_opens() < 0     ? 1 : 0;
    nerrors += test_single_h5repart_opens() < 0     ? 1 : 0;

    if (nerrors)
        goto error;

    HDexit(EXIT_SUCCESS);

error:
    nerrors = MAX(1, nerrors);
    HDprintf("***** %d FAMILY FILE TEST%s FAILED! *****\n",
            nerrors, 1 == nerrors ? "" : "S");
    HDexit(EXIT_FAILURE);
} /* end main() */

