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
 * Purpose:	Test to verify that the infinite loop closing library/abort failure
 *          is fixed when the application creates and removes dense attributes 
 *          (See HDFFV-10659).
 */


#include "h5test.h"

/* The test file name */
const char *FILENAME[] = {
    "del_many_dense_attrs", 
    NULL
};

#define ATTR_COUNT  64          /* The number of attributes */

/*-------------------------------------------------------------------------
 * Function:    catch_signal
 *
 * Purpose:     The signal handler to catch the SIGABRT signal.
 *
 * Return:      No return
 *
 * Programmer:  Vailin Choi
 *
 *-------------------------------------------------------------------------
 */
static void catch_signal(int H5_ATTR_UNUSED signo)
{
    HDexit(EXIT_FAILURE);
} /* catch_signal() */


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Test to verify that the infinite loop closing library/abort failure
 *          is fixed when the application creates and removes dense attributes 
 *          (See HDFFV-10659).
 *
 * Return:	Success:	exit(EXIT_SUCCESS)
 *		    Failure:	exit(EXIT_FAILURE)
 *
 * Programmer:  Vailin Choi; Dec 2018
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t fid = -1;             /* HDF5 File ID                 */
    hid_t gid = -1;             /* Group ID                     */
    hid_t sid = -1;             /* Dataspace ID                 */
    hid_t aid = -1;             /* Attribute ID                 */
    hid_t tid = -1;             /* Datatype ID                  */
    hid_t fapl = -1;            /* File access property lists   */
    hid_t gcpl = -1;            /* Group creation property list */
    char aname[50];             /* Name of attribute            */
    char *basename="attr";      /* Name prefix for attribute    */
    char filename[100];         /* File name                    */
    int i;                      /* Local index variable         */

    /* Testing setup */
    h5_reset();

    /* To exit from the file for SIGABRT signal */
    if(HDsignal(SIGABRT, catch_signal) == SIG_ERR)
       TEST_ERROR

    fapl = h5_fileaccess();
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Set to latest format */
    if(H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        TEST_ERROR

    /* Create the file  */
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(fid) < 0)
        TEST_ERROR

     /* Re-open the file */
    if((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR

    /* Create the group creation property list */
    if((gcpl = H5Pcreate(H5P_GROUP_CREATE)) < 0)
        TEST_ERROR

    /* Set to use dense storage for all attributes on the group */
    if(H5Pset_attr_phase_change(gcpl, 0, 0) < 0)
        TEST_ERROR

    /* Create the group in the file */
    if((gid = H5Gcreate2(fid, "group", H5P_DEFAULT, gcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Create dataspace */
    if((sid = H5Screate(H5S_SCALAR)) < 0)
        TEST_ERROR

    /* Get a copy of the datatype */
    if((tid = H5Tcopy(H5T_NATIVE_FLOAT)) < 0)
        TEST_ERROR

    /* Create attributes in the group */
    for(i = ATTR_COUNT; i >= 0; i--) {
        /* Set up the attribute name */
        HDsprintf(aname, "%s%d", basename, i);

        /* Create the attribute */
        if((aid = H5Acreate2(gid, aname, tid, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0)
            TEST_ERROR

        /* Write to the attribute */
        if(H5Awrite(aid, tid, &i) < 0)
            TEST_ERROR

        /* Close the attribute */
        if(H5Aclose(aid) < 0)
            TEST_ERROR
    }
    
    /* Close the datatype */
    if(H5Tclose(tid) < 0)
       TEST_ERROR

    /* Close the dataspace */
    if(H5Sclose(sid) < 0)
        TEST_ERROR

    /* Close the group */
    if(H5Gclose(gid) < 0)
        TEST_ERROR

    /* Close the group creation property list */
    if(H5Pclose(gcpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(fid) < 0)
        TEST_ERROR

    /* Re-open the file */
    if((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR

    /* Open the group */
    if((gid = H5Gopen2(fid, "group", H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Delete the attributes */
    for (i = 0; i <= ATTR_COUNT; i++) {
        /* Set up the attribute name */
        HDsprintf(aname, "%s%d", basename, i);

        /* Delete the attribute */
        if(H5Adelete(gid, aname) < 0)
            TEST_ERROR
    } /* end for */

    /* Close the group */
    if(H5Gclose(gid) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(fid) < 0)
        TEST_ERROR

    h5_cleanup(FILENAME, fapl);

    return(EXIT_SUCCESS);

error:
    H5E_BEGIN_TRY {
        H5Gclose(gid);
        H5Sclose(sid);
        H5Tclose(tid);
        H5Aclose(aid);
        H5Fclose(fid);
        H5Pclose(gcpl);
        H5Pclose(fapl);
    } H5E_END_TRY

    return EXIT_FAILURE;
}
