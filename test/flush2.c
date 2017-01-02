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
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Friday, October 23, 1998
 *
 * Purpose:	This is the second half of a two-part test that makes sure
 *		that a file can be read after an application crashes as long
 *		as the file was flushed first.  This half tries to read the
 *		file created by the first half.
 */
#include "h5test.h"

/* This file needs to access the file driver testing code */
#define H5FD_FRIEND     /*suppress error about including H5FDpkg      */
#define H5FD_TESTING
#include "H5FDpkg.h"    /* File drivers             */

/* Make this private property (defined in H5Fprivate.h) available */
/* This is used in the helper routine clear_status_flags() */
#define H5F_ACS_CLEAR_STATUS_FLAGS_NAME            "clear_status_flags"


const char *FILENAME[] = {
    "flush",
    "flush-swmr",
    "noflush",
    "noflush-swmr",
    "flush_extend",
    "flush_extend-swmr",
    "noflush_extend",
    "noflush_extend-swmr",
    NULL
};

/* Number and size of dataset dims, chunk size, etc. */
#define NELEMENTS   10000
#define FIRST_DSET_NAME     "dset1"
#define SECOND_DSET_NAME    "dset2"

/* Number of sub-groups created in the containing group */
#define NGROUPS     100

static hbool_t dset_ok(hid_t fid, const char *dset_name);
static hbool_t file_ok(const char *filename, hid_t fapl_id, hbool_t check_second_dset);


/*-------------------------------------------------------------------------
 * Function:    dset_ok
 *
 * Purpose:     Checks if the data in a dataset is what it is supposed to be.
 *
 * Return:      TRUE/FALSE
 *
 * Programmer:	Leon Arber
 *              Oct. 4, 2006.
 *
 *-------------------------------------------------------------------------
 */
static hbool_t
dset_ok(hid_t fid, const char *dset_name)
{
    hid_t   sid = -1;               /* dataspace ID                     */
    hid_t   did = -1;               /* dataset ID                       */
    int    *data = NULL;            /* data buffer                      */
    hsize_t dims[1] = {0};          /* size of dataset                  */
    int     i;                      /* iterator                         */

    /* Open the dataset and check size */
    if((did = H5Dopen2(fid, dset_name, H5P_DEFAULT)) < 0)
        goto error;
    if((sid = H5Dget_space(did)) < 0)
        goto error;
    if(H5Sget_simple_extent_dims(sid, dims, NULL) < 0)
        goto error;
    if(dims[0] != NELEMENTS)
        goto error;

    /* Read the data */
    if(NULL == (data = (int *)HDcalloc((size_t)NELEMENTS, sizeof(int))))
        goto error;
    if(H5Dread(did, H5T_NATIVE_INT, sid, sid, H5P_DEFAULT, data) < 0)
        goto error;
    for(i = 0; i < NELEMENTS; i++)
        if(i != data[i])
            goto error;

    if(H5Sclose(sid) < 0)
        goto error;
    if(H5Dclose(did) < 0)
        goto error;

    HDfree(data);

    return TRUE;

error:
    H5E_BEGIN_TRY {
        H5Sclose(sid);
        H5Dclose(did);
    } H5E_END_TRY;

    HDfree(data);

    return FALSE;
} /* end dset_ok() */


/*-------------------------------------------------------------------------
 * Function:    file_ok
 *
 * Purpose:     Checks that the contents of a file are what they should be.
 *
 * Return:      TRUE/FALSE
 *
 * Programmer:	Leon Arber
 *              Sept. 26, 2006.
 *
 *-------------------------------------------------------------------------
 */
static hbool_t
file_ok(const char *filename, hid_t fapl_id, hbool_t check_second_dset)
{
    hid_t   fid = -1;               /* file ID                          */
    hid_t   top_gid = -1;           /* containing group ID              */
    hid_t   gid = -1;               /* subgroup ID                      */
    char    group_name[16];         /* group name                       */
    int     i;                      /* iterator                         */

    /* open file */
    if((fid = H5Fopen(filename, H5F_ACC_RDONLY, fapl_id)) < 0)
        goto error;

    /* check datasets */
    if(!dset_ok(fid, FIRST_DSET_NAME))
        goto error;
    if(check_second_dset)
        if(!dset_ok(fid, SECOND_DSET_NAME))
            goto error;

    /* check groups */
    if((top_gid = H5Gopen2(fid, "top_group", H5P_DEFAULT)) < 0)
        goto error;
    for(i = 0; i < NGROUPS; i++) {
        HDsprintf(group_name, "group%02d", i);
        if((gid = H5Gopen2(top_gid, group_name, H5P_DEFAULT)) < 0)
            goto error;
        if(H5Gclose(gid) < 0)
            goto error;
    } /* end for */

    if(H5Gclose(top_gid) < 0)
        goto error;
    if(H5Fclose(fid) < 0)
        goto error;

    return TRUE;

error:
    H5E_BEGIN_TRY {
        H5Gclose(top_gid);
        H5Gclose(gid);
        H5Fclose(fid);
    } H5E_END_TRY;

    return FALSE;
} /* end file_ok() */

/*-------------------------------------------------------------------------
 * Function:    clear_status_flags
 *
 * Purpose:     Clears the status_flags in the superblock of the file.
 *              It is smilar to the tool "h5clear".
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Vailin Choi
 *              July 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
clear_status_flags(const char *filename, hid_t fapl_id)
{
    hid_t   new_fapl_id = -1;       /* copy of the file access plist ID */
    hid_t   fid = -1;               /* file ID                          */
    hbool_t clear = TRUE;

    /* Get a copy of fapl */
    if((new_fapl_id = H5Pcopy(fapl_id)) < 0)
        goto error;

    /* Set this private property */
    if(H5Pset(new_fapl_id, H5F_ACS_CLEAR_STATUS_FLAGS_NAME, &clear) < 0)
        goto error;

    /* Has to open rw */
    if((fid = H5Fopen(filename, H5F_ACC_RDWR, new_fapl_id)) < 0)
        goto error;

    /* CLose the property list */
    if(H5Pclose(new_fapl_id) < 0)
        goto error;

    /* Close the file */
    if(H5Fclose(fid) < 0)
        goto error;

    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Pclose(new_fapl_id);
        H5Fclose(fid);
    } H5E_END_TRY;

    return FAIL;
} /* end clear_status_flags() */


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:     Confirms file contents in a variety of flush scenarios.
 *              Part 2 of a two-part H5Fflush() test.
 *
 * Return:      EXIT_SUCCESS/EXIT_FAILURE
 *
 * Programmer:	Robb Matzke
 *              Friday, October 23, 1998
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    char   *driver = NULL;      /* name of current VFD (from env var)       */
    hbool_t vfd_supports_swmr;  /* whether the current VFD supports SWMR    */
    hid_t   fapl_id = -1;       /* file access proplist ID                  */
    char    filename[1024];     /* filename                                 */
    hbool_t check_second_dset;  /* whether or not to check the second dset  */
    H5E_auto2_t func;           /* for shutting off error reporting         */

    h5_reset();
    if((fapl_id = h5_fileaccess()) < 0)
        PUTS_ERROR("bad vfd-dependent fapl")

    /* Check if the current VFD supports SWMR */
    driver = HDgetenv("HDF5_DRIVER");
    vfd_supports_swmr = H5FD_supports_swmr_test(driver);

    /* TEST 1 */
    /* Check the case where the file was flushed */
    TESTING("H5Fflush (part2 with flush)");
    h5_fixname(FILENAME[0], fapl_id, filename, sizeof(filename));
    check_second_dset = FALSE;
    if(file_ok(filename, fapl_id, check_second_dset))
        PASSED()
    else
        TEST_ERROR

    /* TEST 2 */
    /* Check the case where the file was flushed (w/SWMR) */
    TESTING("H5Fflush (part2 with flush + SWMR)");
    if(vfd_supports_swmr) {
        h5_fixname(FILENAME[1], fapl_id, filename, sizeof(filename));
        check_second_dset = FALSE;
        if(clear_status_flags(filename, fapl_id) < 0)
            TEST_ERROR
        if(file_ok(filename, fapl_id, check_second_dset))
            PASSED()
        else
            TEST_ERROR
    } /* end if */
    else
        SKIPPED()

    /* TEST 3 */
    /* Check the case where the file was not flushed */
    TESTING("H5Fflush (part2 without flush)");
    /* Turn the error stack off (failures expected) */
    if(H5Eget_auto2(H5E_DEFAULT, &func, NULL) < 0)
        FAIL_STACK_ERROR
    if(H5Eset_auto2(H5E_DEFAULT, NULL, NULL) < 0)
        FAIL_STACK_ERROR
    h5_fixname(FILENAME[2], fapl_id, filename, sizeof(filename));
    check_second_dset = FALSE;
    if(file_ok(filename, fapl_id, check_second_dset)) {
#if defined H5_HAVE_WIN32_API && !defined (hdf5_EXPORTS)
        SKIPPED();
        HDputs("   the DLL will flush the file even when calling _exit, skip this test temporarily");
#else
        TEST_ERROR
#endif
    } /* end if */
    else
        PASSED()
    /* Turn the error stack back on */
    if(H5Eset_auto2(H5E_DEFAULT, func, NULL) < 0)
        FAIL_STACK_ERROR

    /* TEST 4 */
    /* Check the case where the file was not flushed (w/SWMR) */
    TESTING("H5Fflush (part2 without flush + SWMR)");
    if(vfd_supports_swmr) {
        /* Turn the error stack off (failures expected) */
        if(H5Eget_auto2(H5E_DEFAULT, &func, NULL) < 0)
            FAIL_STACK_ERROR
        if(H5Eset_auto2(H5E_DEFAULT, NULL, NULL) < 0)
            FAIL_STACK_ERROR
        h5_fixname(FILENAME[3], fapl_id, filename, sizeof(filename));
        check_second_dset = FALSE;
        if(clear_status_flags(filename, fapl_id) < 0)
            TEST_ERROR
        if(file_ok(filename, fapl_id, check_second_dset)) {
#if defined H5_HAVE_WIN32_API && !defined (hdf5_EXPORTS)
            SKIPPED();
            HDputs("   the DLL will flush the file even when calling _exit, skip this test temporarily");
#else
            TEST_ERROR
#endif
        } /* end if */
        else
            PASSED()
        /* Turn the error stack back on */
        if(H5Eset_auto2(H5E_DEFAULT, func, NULL) < 0)
            FAIL_STACK_ERROR
    } /* end if */
    else
        SKIPPED()

    /* TEST 5 */
    /* Check the case where the file was flushed, but more data was
     * added afterward and then flushed
     */
    TESTING("H5Fflush (part2 with flush and later addition and another flush)");
    check_second_dset = TRUE;
    h5_fixname(FILENAME[4], fapl_id, filename, sizeof(filename));
    if(file_ok(filename, fapl_id, check_second_dset))
        PASSED()
    else
        TEST_ERROR

    /* TEST 6 */
    /* Check the case where the file was flushed, but more data was
     * added afterward and then flushed (w/SWMR) 
     */
    TESTING("H5Fflush (part2 with flush and later addition and another flush + SWMR)");
    if(vfd_supports_swmr) {
        check_second_dset = TRUE;
        h5_fixname(FILENAME[5], fapl_id, filename, sizeof(filename));
        if(clear_status_flags(filename, fapl_id) < 0)
            TEST_ERROR
        if(file_ok(filename, fapl_id, check_second_dset))
            PASSED()
        else
            TEST_ERROR
    } /* end if */
    else
        SKIPPED()

    /* TEST 7 */
    /* Check the case where the file was flushed, but more data was added
     * afterward and not flushed.
     */
    TESTING("H5Fflush (part2 with flush and later addition)");
    /* Turn the error stack off (failures expected) */
    if(H5Eget_auto2(H5E_DEFAULT, &func, NULL) < 0)
        FAIL_STACK_ERROR
    if(H5Eset_auto2(H5E_DEFAULT, NULL, NULL) < 0)
        FAIL_STACK_ERROR
    h5_fixname(FILENAME[6], fapl_id, filename, sizeof(filename));
    check_second_dset = TRUE;
    if(file_ok(filename, fapl_id, check_second_dset)) {
#if defined H5_HAVE_WIN32_API && !defined (hdf5_EXPORTS)
        SKIPPED()
        HDputs("   the DLL will flush the file even when calling _exit, skip this test temporarily");
#else
        TEST_ERROR
#endif
    } /* end if */
    else
        PASSED()
    /* Turn the error stack back on */
    if(H5Eset_auto2(H5E_DEFAULT, func, NULL) < 0)
        FAIL_STACK_ERROR

    /* TEST 8 */
    /* Check the case where the file was flushed, but more data was added
     * afterward and not flushed (w/ SWMR).
     */
    TESTING("H5Fflush (part2 with flush and later addition + SWMR)");
    if(vfd_supports_swmr) {
        /* Turn the error stack off (failures expected) */
        if(H5Eget_auto2(H5E_DEFAULT, &func, NULL) < 0)
            FAIL_STACK_ERROR
        if(H5Eset_auto2(H5E_DEFAULT, NULL, NULL) < 0)
            FAIL_STACK_ERROR
        h5_fixname(FILENAME[7], fapl_id, filename, sizeof(filename));
        check_second_dset = TRUE;
        if(clear_status_flags(filename, fapl_id) < 0)
            TEST_ERROR
        if(file_ok(filename, fapl_id, check_second_dset)) {
#if defined H5_HAVE_WIN32_API && !defined (hdf5_EXPORTS)
            SKIPPED();
            HDputs("   the DLL will flush the file even when calling _exit, skip this test temporarily");
#else
            TEST_ERROR
#endif
        } /* end if */
        else
            PASSED();
        /* Turn the error stack back on */
        if(H5Eset_auto2(H5E_DEFAULT, func, NULL) < 0)
            FAIL_STACK_ERROR
    } /* end if */
    else
        SKIPPED()

    if(!vfd_supports_swmr)
        HDprintf("NOTE: Some tests were skipped since the current VFD lacks SWMR support\n");

    h5_cleanup(FILENAME, fapl_id);

    HDexit(EXIT_SUCCESS);

error:
    HDexit(EXIT_FAILURE);
} /* end main() */

