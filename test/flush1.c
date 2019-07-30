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
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Friday, October 23, 1998
 *
 * Purpose:	This is the first half of a two-part test that makes sure
 *		that a file can be read after an application crashes as long
 *		as the file was flushed first.  We simulate a crash by
 *		calling _exit(EXIT_SUCCESS) since this doesn't flush HDF5 caches but
 *		still exits with success.
 */
#include "h5test.h"

/* This file needs to access the file driver testing code */
#define H5FD_FRIEND     /*suppress error about including H5FDpkg      */
#define H5FD_TESTING
#include "H5FDpkg.h"    /* File drivers             */

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
#define NDIMS       1
#define NELEMENTS   10000
#define CHUNK_SIZE  25
#define FIRST_DSET_NAME     "dset1"
#define SECOND_DSET_NAME    "dset2"

/* Number of sub-groups created in the containing group */
#define NGROUPS     100

static hid_t create_file(const char *filename, hid_t fapl_id, hbool_t swmr);
static herr_t add_dset_to_file(hid_t fid, const char *dset_name);


/*-------------------------------------------------------------------------
 * Function:    create_file
 *
 * Purpose:     Creates files and datasets used in part 1 of the test
 *
 * Return:      Success:	a valid file ID
 *              Failure:	-1
 *
 * Programmer:	Leon Arber
 *              Sept. 26, 2006
 *
 *-------------------------------------------------------------------------
 */
static hid_t
create_file(const char *filename, hid_t fapl_id, hbool_t swmr)
{
    hid_t   fid = -1;               /* file ID                          */
    hid_t   top_gid = -1;           /* containing group ID              */
    hid_t   gid = -1;               /* subgroup ID                      */
    char    group_name[32];         /* group name                       */
    unsigned    flags;              /* file open flags                  */
    int     i;                      /* iterator                         */

    flags = H5F_ACC_TRUNC | (swmr ? H5F_ACC_SWMR_WRITE : 0);

    if((fid = H5Fcreate(filename, flags, H5P_DEFAULT, fapl_id)) < 0)
        STACK_ERROR

    /* Create a chunked dataset */
    if(add_dset_to_file(fid, FIRST_DSET_NAME) < 0)
        TEST_ERROR

    /* Create some groups */
    if((top_gid = H5Gcreate2(fid, "top_group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        STACK_ERROR
    for(i = 0; i < NGROUPS; i++) {
        HDsprintf(group_name, "group%02d", i);
        if((gid = H5Gcreate2(top_gid, group_name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
            STACK_ERROR
        if(H5Gclose(gid) < 0)
            STACK_ERROR
    } /* end for */

    if(H5Gclose(top_gid) < 0)
        STACK_ERROR
    
    return fid;

error:
    H5E_BEGIN_TRY {
        H5Fclose(fid);
        H5Gclose(gid);
        H5Gclose(top_gid);
     } H5E_END_TRY;

    return -1;
} /* end create_file() */


/*-------------------------------------------------------------------------
 * Function:    add_dset_to_file
 *
 * Purpose:     Add a dataset to the file.
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:	Leon Arber
 *              Oct. 4, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
add_dset_to_file(hid_t fid, const char *dset_name)
{
    hid_t   dcpl_id = -1;           /* dataset creation plist ID        */
    hid_t   sid = -1;               /* dataspace ID                     */
    hid_t   did = -1;               /* dataset ID                       */
    int    *data = NULL;            /* data buffer                      */
    hsize_t dims[1] = {NELEMENTS};  /* size of dataset                  */
    hsize_t chunk_dims[1] = {CHUNK_SIZE};   /* chunk size               */
    int     i;                      /* iterator                         */

    /* Create a chunked dataset */
    if((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        STACK_ERROR
    if(H5Pset_chunk(dcpl_id, NDIMS, chunk_dims) < 0)
        STACK_ERROR
    if((sid = H5Screate_simple(NDIMS, dims, NULL)) < 0)
        STACK_ERROR
    if((did = H5Dcreate2(fid, dset_name, H5T_NATIVE_FLOAT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        STACK_ERROR

    /* Write some data */
    if(NULL == (data = (int *)HDcalloc((size_t)NELEMENTS, sizeof(int))))
        STACK_ERROR
    for(i = 0; i < NELEMENTS; i++)
        data[i] = i;
    if(H5Dwrite(did, H5T_NATIVE_INT, sid, sid, H5P_DEFAULT, data) < 0)
        STACK_ERROR

    if(H5Pclose(dcpl_id) < 0)
        STACK_ERROR
    if(H5Sclose(sid) < 0)
        STACK_ERROR
    if(H5Dclose(did) < 0)
        STACK_ERROR

    HDfree(data);

    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl_id);
        H5Sclose(sid);
        H5Dclose(did);
     } H5E_END_TRY;

    HDfree(data);

    return FAIL;
} /* end add_dset_to_file() */


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:     Creates files and datasets with and without flushing in
 *              a variety of situations.
 *
 *              Part 1 of a two-part H5Fflush() test.
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
    hid_t   fid = -1;           /* file ID                                  */
    hid_t   fapl_id = -1;       /* file access proplist ID                  */
    char    filename[1024];     /* filename                                 */
    hbool_t use_swmr;           /* whether or not to use SWMR I/O           */

    h5_reset();
    if((fapl_id = h5_fileaccess()) < 0)
        TEST_ERROR

    /* Check if the current VFD supports SWMR */
    driver = HDgetenv("HDF5_DRIVER");
    vfd_supports_swmr = H5FD_supports_swmr_test(driver);

    /*************************************************/
    /* NOTE: Not closing the file ID is intentional! */
    /*************************************************/

    /* Create a file and flush */
    TESTING("H5Fflush (part1 with flush)");
    h5_fixname(FILENAME[0], fapl_id, filename, sizeof(filename));
    use_swmr = FALSE;
    if((fid = create_file(filename, fapl_id, use_swmr)) < 0)
        TEST_ERROR
    if(H5Fflush(fid, H5F_SCOPE_GLOBAL) < 0)
        FAIL_STACK_ERROR
    PASSED();

    /* Create a file and flush w/ SWMR I/O */
    TESTING("H5Fflush (part1 with flush + SWMR)");
    if(vfd_supports_swmr) {
        h5_fixname(FILENAME[1], fapl_id, filename, sizeof(filename));
        use_swmr = TRUE;
        if((fid = create_file(filename, fapl_id, use_swmr)) < 0)
            TEST_ERROR
        if(H5Fflush(fid, H5F_SCOPE_GLOBAL) < 0)
            FAIL_STACK_ERROR
        PASSED();
    } /* end if */
    else
        SKIPPED();

    /* Create a file which will not be flushed */
    TESTING("H5Fflush (part1 without flush)");
    h5_fixname(FILENAME[2], fapl_id, filename, sizeof(filename));
    use_swmr = FALSE;
    if((fid = create_file(filename, fapl_id, use_swmr)) < 0)
        TEST_ERROR
    PASSED();

    /* Create a file which will not be flushed w/ SWMR I/O */
    TESTING("H5Fflush (part1 without flush + SWMR)");
    if(vfd_supports_swmr) {
        h5_fixname(FILENAME[3], fapl_id, filename, sizeof(filename));
        use_swmr = TRUE;
        if((fid = create_file(filename, fapl_id, use_swmr)) < 0)
            TEST_ERROR
        PASSED();
    } /* end if */
    else
        SKIPPED();

    /* Create a file, flush, add a dataset, flush */
    TESTING("H5Fflush (part1 with flush and later addition and another flush)");
    h5_fixname(FILENAME[4], fapl_id, filename, sizeof(filename));
    use_swmr = FALSE;
    if((fid = create_file(filename, fapl_id, use_swmr)) < 0)
        TEST_ERROR
    if(H5Fflush(fid, H5F_SCOPE_GLOBAL) < 0)
        FAIL_STACK_ERROR
    if(add_dset_to_file(fid, SECOND_DSET_NAME) < 0)
        TEST_ERROR
    if(H5Fflush(fid, H5F_SCOPE_GLOBAL) < 0)
        FAIL_STACK_ERROR
    PASSED();

    /* Create a file, flush, add a dataset, flush w/ SWMR I/O */
    TESTING("H5Fflush (part1 with flush and later addition and another flush + SWMR)");
    if(vfd_supports_swmr) {
        h5_fixname(FILENAME[5], fapl_id, filename, sizeof(filename));
        use_swmr = TRUE;
        if((fid = create_file(filename, fapl_id, use_swmr)) < 0)
            TEST_ERROR
        if(H5Fflush(fid, H5F_SCOPE_GLOBAL) < 0)
            FAIL_STACK_ERROR
        if(add_dset_to_file(fid, SECOND_DSET_NAME) < 0)
            TEST_ERROR
        if(H5Fflush(fid, H5F_SCOPE_GLOBAL) < 0)
            FAIL_STACK_ERROR
        PASSED();
    } /* end if */
    else
        SKIPPED();

    /* Create a file, flush, add a dataset, (no flush) */
    TESTING("H5Fflush (part1 with flush and later addition)");
    h5_fixname(FILENAME[6], fapl_id, filename, sizeof(filename));
    use_swmr = FALSE;
    if((fid = create_file(filename, fapl_id, use_swmr)) < 0)
        TEST_ERROR
    if(H5Fflush(fid, H5F_SCOPE_GLOBAL) < 0)
        FAIL_STACK_ERROR
    if(add_dset_to_file(fid, SECOND_DSET_NAME) < 0)
        TEST_ERROR
    PASSED();

    /* Create a file, flush, add a dataset, (no flush) w/ SWMR I/O */
    TESTING("H5Fflush (part1 with flush and later addition + SWMR)");
    if(vfd_supports_swmr) {
        h5_fixname(FILENAME[7], fapl_id, filename, sizeof(filename));
        use_swmr = TRUE;
        if((fid = create_file(filename, fapl_id, use_swmr)) < 0)
            TEST_ERROR
        if(H5Fflush(fid, H5F_SCOPE_GLOBAL) < 0)
            FAIL_STACK_ERROR
        if(add_dset_to_file(fid, SECOND_DSET_NAME) < 0)
            TEST_ERROR
        PASSED();
    } /* end if */
    else
        SKIPPED();

    if(!vfd_supports_swmr)
        HDprintf("NOTE: Some tests were skipped since the current VFD lacks SWMR support\n");

    /* Flush console output streams */
    HDfflush(stdout);
    HDfflush(stderr);

    /* DO NOT CLOSE FILE ID! */
    if(H5Pclose(fapl_id) < 0)
        STACK_ERROR

    /* _exit() is necessary since we want a hard close of the library */
    HD_exit(EXIT_SUCCESS);

error:
    H5E_BEGIN_TRY {
        H5Pclose(fapl_id);
    } H5E_END_TRY;

    HDexit(EXIT_FAILURE);
} /* end main() */

