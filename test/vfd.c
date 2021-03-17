/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Programmer:  Raymond Lu
 *              Tuesday, Sept 24, 2002
 *
 * Purpose:     Tests the basic features of Virtual File Drivers
 */

#include "h5test.h"

#define KB            1024U
#define FAMILY_NUMBER 4
#define FAMILY_SIZE   (1 * KB)
#define FAMILY_SIZE2  (5 * KB)
#define MULTI_SIZE    128
#define SPLITTER_SIZE 8 /* dimensions of a dataset */

#define CORE_INCREMENT (4 * KB)
#define CORE_PAGE_SIZE (1024 * KB)
#define CORE_DSET_NAME "core dset"
#define CORE_DSET_DIM1 1024
#define CORE_DSET_DIM2 32

#define DSET1_NAME "dset1"
#define DSET1_DIM1 1024
#define DSET1_DIM2 32
#define DSET3_NAME "dset3"

/* Macros for Direct VFD */
#ifdef H5_HAVE_DIRECT
#define MBOUNDARY  512
#define FBSIZE     (4 * KB)
#define CBSIZE     (8 * KB)
#define THRESHOLD  1
#define DSET2_NAME "dset2"
#define DSET2_DIM  4
#endif /* H5_HAVE_DIRECT */

const char *FILENAME[] = {"sec2_file",          /*0*/
                          "core_file",          /*1*/
                          "family_file",        /*2*/
                          "new_family_v16_",    /*3*/
                          "multi_file",         /*4*/
                          "direct_file",        /*5*/
                          "log_file",           /*6*/
                          "stdio_file",         /*7*/
                          "windows_file",       /*8*/
                          "new_multi_file_v16", /*9*/
                          "ro_s3_file",         /*10*/
                          "splitter_rw_file",   /*11*/
                          "splitter_wo_file",   /*12*/
                          "splitter.log",       /*13*/
                          NULL};

#define LOG_FILENAME "log_vfd_out.log"

#define COMPAT_BASENAME       "family_v16_"
#define MULTI_COMPAT_BASENAME "multi_file_v16"
#define SPLITTER_DATASET_NAME "dataset"

/* Macro: HEXPRINT()
 * Helper macro to pretty-print hexadecimal output of a buffer of known size.
 * Each line has the address of the first printed byte, and four columns of
 * four-byte data.
 */
static int __k;
#define HEXPRINT(size, buf)                                                                                  \
    for (__k = 0; __k < (size); __k++) {                                                                     \
        if (__k % 16 == 0) {                                                                                 \
            HDprintf("\n%04x", __k);                                                                         \
        }                                                                                                    \
        HDprintf((__k % 4 == 0) ? "  %02X" : " %02X", (unsigned char)(buf)[__k]);                            \
    } /* end #define HEXPRINT() */

/* Helper structure to pass around dataset information.
 */
struct splitter_dataset_def {
    void *         buf;         /* contents of dataset */
    const char *   dset_name;   /* dataset name, always added to root group */
    hid_t          mem_type_id; /* datatype */
    const hsize_t *dims;        /* dimensions */
    int            n_dims;      /* rank */
};

static int splitter_prepare_file_paths(H5FD_splitter_vfd_config_t *vfd_config, char *filename_rw_out);
static int splitter_create_single_file_at(const char *filename, hid_t fapl_id,
                                          const struct splitter_dataset_def *data);
static int splitter_compare_expected_data(hid_t file_id, const struct splitter_dataset_def *data);
static int run_splitter_test(const struct splitter_dataset_def *data, hbool_t ignore_wo_errors,
                             hbool_t provide_logfile_path, const hid_t sub_fapl_ids[2]);
static int splitter_RO_test(const struct splitter_dataset_def *data, hid_t child_fapl_id);
static int splitter_tentative_open_test(hid_t child_fapl_id);
static int file_exists(const char *filename, hid_t fapl_id);

/*-------------------------------------------------------------------------
 * Function:    test_sec2
 *
 * Purpose:     Tests the file handle interface for SEC2 driver
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Raymond Lu
 *              Tuesday, Sept 24, 2002
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_sec2(void)
{
    hid_t         fid          = -1;     /* file ID                      */
    hid_t         fapl_id      = -1;     /* file access property list ID */
    hid_t         fapl_id_out  = -1;     /* from H5Fget_access_plist     */
    hid_t         driver_id    = -1;     /* ID for this VFD              */
    unsigned long driver_flags = 0;      /* VFD feature flags            */
    char          filename[1024];        /* filename                     */
    void *        os_file_handle = NULL; /* OS file handle               */
    hsize_t       file_size;             /* file size                    */

    TESTING("SEC2 file driver");

    /* Set property list and file name for SEC2 driver. */
    if ((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;
    if (H5Pset_fapl_sec2(fapl_id) < 0)
        TEST_ERROR;
    h5_fixname(FILENAME[0], fapl_id, filename, sizeof(filename));

    /* Check that the VFD feature flags are correct */
    if ((driver_id = H5Pget_driver(fapl_id)) < 0)
        TEST_ERROR
    if (H5FDdriver_query(driver_id, &driver_flags) < 0)
        TEST_ERROR
    if (!(driver_flags & H5FD_FEAT_AGGREGATE_METADATA))
        TEST_ERROR
    if (!(driver_flags & H5FD_FEAT_ACCUMULATE_METADATA))
        TEST_ERROR
    if (!(driver_flags & H5FD_FEAT_DATA_SIEVE))
        TEST_ERROR
    if (!(driver_flags & H5FD_FEAT_AGGREGATE_SMALLDATA))
        TEST_ERROR
    if (!(driver_flags & H5FD_FEAT_POSIX_COMPAT_HANDLE))
        TEST_ERROR
    if (!(driver_flags & H5FD_FEAT_SUPPORTS_SWMR_IO))
        TEST_ERROR
    if (!(driver_flags & H5FD_FEAT_DEFAULT_VFD_COMPATIBLE))
        TEST_ERROR
    /* Check for extra flags not accounted for above */
    if (driver_flags != (H5FD_FEAT_AGGREGATE_METADATA | H5FD_FEAT_ACCUMULATE_METADATA | H5FD_FEAT_DATA_SIEVE |
                         H5FD_FEAT_AGGREGATE_SMALLDATA | H5FD_FEAT_POSIX_COMPAT_HANDLE |
                         H5FD_FEAT_SUPPORTS_SWMR_IO | H5FD_FEAT_DEFAULT_VFD_COMPATIBLE))
        TEST_ERROR

    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id)) < 0)
        TEST_ERROR;

    /* Retrieve the access property list... */
    if ((fapl_id_out = H5Fget_access_plist(fid)) < 0)
        TEST_ERROR;

    /* Check that the driver is correct */
    if (H5FD_SEC2 != H5Pget_driver(fapl_id_out))
        TEST_ERROR;

    /* ...and close the property list */
    if (H5Pclose(fapl_id_out) < 0)
        TEST_ERROR;

    /* Check that we can get an operating-system-specific handle from
     * the library.
     */
    if (H5Fget_vfd_handle(fid, H5P_DEFAULT, &os_file_handle) < 0)
        TEST_ERROR;
    if (os_file_handle == NULL)
        FAIL_PUTS_ERROR("NULL os-specific vfd/file handle was returned from H5Fget_vfd_handle");

    /* There is no garantee the size of metadata in file is constant.
     * Just try to check if it's reasonable.
     *
     * Currently it should be around 2 KB.
     */
    if (H5Fget_filesize(fid, &file_size) < 0)
        TEST_ERROR;
    if (file_size < 1 * KB || file_size > 4 * KB)
        FAIL_PUTS_ERROR("suspicious file size obtained from H5Fget_filesize");

    /* Close and delete the file */
    if (H5Fclose(fid) < 0)
        TEST_ERROR;
    h5_delete_test_file(FILENAME[0], fapl_id);

    /* Close the fapl */
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(fapl_id);
        H5Pclose(fapl_id_out);
        H5Fclose(fid);
    }
    H5E_END_TRY;
    return -1;
} /* end test_sec2() */

/*-------------------------------------------------------------------------
 * Function:    test_core
 *
 * Purpose:     Tests the file handle interface for CORE driver
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Raymond Lu
 *              Tuesday, Sept 24, 2002
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_core(void)
{
    hid_t         fid          = -1;        /* file ID                      */
    hid_t         fapl_id      = -1;        /* file access property list ID */
    hid_t         fapl_id_out  = -1;        /* from H5Fget_access_plist     */
    hid_t         driver_id    = -1;        /* ID for this VFD              */
    unsigned long driver_flags = 0;         /* VFD feature flags            */
    hid_t         did          = -1;        /* dataset ID                   */
    hid_t         sid          = -1;        /* dataspace ID                 */
    char          filename[1024];           /* filename                     */
    void *        os_file_handle = NULL;    /* OS file handle               */
    hsize_t       file_size;                /* file size                    */
    size_t        increment;                /* core VFD increment           */
    hbool_t       backing_store;            /* use backing store?           */
    hbool_t       use_write_tracking;       /* write tracking flag          */
    size_t        write_tracking_page_size; /* write tracking page size     */
    int *         data_w = NULL;            /* data written to the dataset  */
    int *         data_r = NULL;            /* data read from the dataset   */
    int           val;                      /* data value                   */
    int *         pw = NULL, *pr = NULL;    /* pointers for iterating over
                                               data arrays (write & read)   */
    hsize_t dims[2];                        /* dataspace dimensions         */
    int     i, j;                           /* iterators                    */
    htri_t  status;                         /* return value from H5Lexists  */

    TESTING("CORE file driver");

    /* Get a file access property list and fix up the file name */
    if ((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;
    h5_fixname(FILENAME[1], fapl_id, filename, sizeof(filename));

    /************************************************************************
     * Check that the backing store flag works by creating a file, close
     * it, and ensure that the file does not exist.
     ************************************************************************/

    /* Make sure it's not present at the start of the test */
    if (HDaccess(filename, F_OK) != -1)
        if (HDremove(filename) < 0)
            FAIL_PUTS_ERROR("unable to remove backing store file");

    /* Create and close file w/ backing store off */
    if (H5Pset_fapl_core(fapl_id, (size_t)CORE_INCREMENT, FALSE) < 0)
        TEST_ERROR;

    /* Check that the VFD feature flags are correct.
     * Note that the H5FDdriver_query() API call does not require a file
     * so backing-store related flags will not be returned here.
     */
    if ((driver_id = H5Pget_driver(fapl_id)) < 0)
        TEST_ERROR
    if (H5FDdriver_query(driver_id, &driver_flags) < 0)
        TEST_ERROR
    if (!(driver_flags & H5FD_FEAT_AGGREGATE_METADATA))
        TEST_ERROR
    if (!(driver_flags & H5FD_FEAT_ACCUMULATE_METADATA))
        TEST_ERROR
    if (!(driver_flags & H5FD_FEAT_DATA_SIEVE))
        TEST_ERROR
    if (!(driver_flags & H5FD_FEAT_AGGREGATE_SMALLDATA))
        TEST_ERROR
    if (!(driver_flags & H5FD_FEAT_ALLOW_FILE_IMAGE))
        TEST_ERROR
    if (!(driver_flags & H5FD_FEAT_CAN_USE_FILE_IMAGE_CALLBACKS))
        TEST_ERROR
    /* Check for extra flags not accounted for above */
    if (driver_flags !=
        (H5FD_FEAT_AGGREGATE_METADATA | H5FD_FEAT_ACCUMULATE_METADATA | H5FD_FEAT_DATA_SIEVE |
         H5FD_FEAT_AGGREGATE_SMALLDATA | H5FD_FEAT_ALLOW_FILE_IMAGE | H5FD_FEAT_CAN_USE_FILE_IMAGE_CALLBACKS))
        TEST_ERROR

    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id)) < 0)
        TEST_ERROR;
    if (H5Fclose(fid) < 0)
        TEST_ERROR;
    /* Check for the backing store file */
    if (HDaccess(filename, F_OK) != -1)
        FAIL_PUTS_ERROR("file created when backing store set to FALSE");

    /************************************************************************
     * Check basic core VFD operation and properties. This is done with the
     * backing store on so a file will be created for later use.
     ************************************************************************/

    /* Turn the backing store on */
    if (H5Pset_fapl_core(fapl_id, (size_t)CORE_INCREMENT, TRUE) < 0)
        TEST_ERROR;

    /* Check that write tracking is off by default and that the default
     * page size is non-zero.
     */
    if (H5Pget_core_write_tracking(fapl_id, &use_write_tracking, &write_tracking_page_size) < 0)
        TEST_ERROR;
    if (FALSE != use_write_tracking)
        FAIL_PUTS_ERROR("write tracking should be off by default");
    if (0 == write_tracking_page_size)
        FAIL_PUTS_ERROR("write tracking page size should never be zero");

    /* Set core VFD properties */
    if (H5Pset_core_write_tracking(fapl_id, TRUE, CORE_PAGE_SIZE) < 0)
        TEST_ERROR;

    /* Create the file */
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id)) < 0)
        TEST_ERROR;

    /* Retrieve the access property list... */
    if ((fapl_id_out = H5Fget_access_plist(fid)) < 0)
        TEST_ERROR;

    /* Check that the driver is correct */
    if (H5FD_CORE != H5Pget_driver(fapl_id_out))
        TEST_ERROR;

    /* Get the basic VFD properties from the fapl and ensure that
     * they are correct.
     */
    if (H5Pget_fapl_core(fapl_id_out, &increment, &backing_store) < 0)
        TEST_ERROR
    if (increment != (size_t)CORE_INCREMENT)
        FAIL_PUTS_ERROR("incorrect increment from file fapl");
    if (backing_store != TRUE)
        FAIL_PUTS_ERROR("incorrect backing store flag from file fapl");

    /* Check that the backing store write tracking info was saved */
    /* TODO: There is a bug where H5Fget_access_plist() does not return
     *       the write tracking properties. Until this bug is fixed, just
     *       test the main fapl_id.
     */
    if (H5Pget_core_write_tracking(fapl_id, &use_write_tracking, &write_tracking_page_size) < 0)
        TEST_ERROR;
    if (TRUE != use_write_tracking)
        FAIL_PUTS_ERROR("write tracking flag incorrect in fapl obtained from H5Fget_access_plist");
    if (CORE_PAGE_SIZE != write_tracking_page_size)
        FAIL_PUTS_ERROR("write tracking page size incorrect in fapl obtained from H5Fget_access_plist");

    /* Close the property list */
    if (H5Pclose(fapl_id_out) < 0)
        TEST_ERROR;

    /* Check that we can get an operating-system-specific handle from
     * the library.
     */
    if (H5Fget_vfd_handle(fid, H5P_DEFAULT, &os_file_handle) < 0)
        TEST_ERROR;
    if (os_file_handle == NULL)
        FAIL_PUTS_ERROR("NULL os-specific vfd/file handle was returned from H5Fget_vfd_handle");

    /* There is no garantee the size of metadata in file is constant.
     * Just try to check if it's reasonable.
     *
     * TODO: Needs justification of why is this is a reasonable size.
     */
    if (H5Fget_filesize(fid, &file_size) < 0)
        TEST_ERROR;
    if (file_size < 2 * KB || file_size > 6 * KB)
        FAIL_PUTS_ERROR("suspicious file size obtained from H5Fget_filesize");

    /* Close the file */
    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    /************************************************************************
     * Make changes to the file with the backing store flag OFF to ensure
     * that they ARE NOT propagated.
     ************************************************************************/

    /* Open the file with backing store off for read and write.
     * Changes won't be saved in file.
     */
    if (H5Pset_fapl_core(fapl_id, (size_t)CORE_INCREMENT, FALSE) < 0)
        TEST_ERROR;
    if ((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl_id)) < 0)
        TEST_ERROR;

    /* Allocate memory for data set. */
    if (NULL == (data_w = (int *)HDmalloc(DSET1_DIM1 * DSET1_DIM2 * sizeof(int))))
        FAIL_PUTS_ERROR("unable to allocate memory for input array");
    if (NULL == (data_r = (int *)HDmalloc(DSET1_DIM1 * DSET1_DIM2 * sizeof(int))))
        FAIL_PUTS_ERROR("unable to allocate memory for output array");

    /* Initialize the buffers */
    val = 0;
    pw  = data_w;
    for (i = 0; i < CORE_DSET_DIM1; i++)
        for (j = 0; j < CORE_DSET_DIM2; j++)
            *pw++ = val++;
    HDmemset(data_r, 0, DSET1_DIM1 * DSET1_DIM2 * sizeof(int));

    /* Create the dataspace */
    dims[0] = CORE_DSET_DIM1;
    dims[1] = CORE_DSET_DIM2;
    if ((sid = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;

    /* Create the dataset */
    if ((did = H5Dcreate2(fid, CORE_DSET_NAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) <
        0)
        TEST_ERROR;

    /* Write the data to the dataset */
    if (H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data_w) < 0)
        TEST_ERROR;

    /* Close and reopen the dataset */
    if (H5Dclose(did) < 0)
        TEST_ERROR;
    if ((did = H5Dopen2(fid, CORE_DSET_NAME, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Read the data back from dset1 */
    if (H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data_r) < 0)
        TEST_ERROR;

    /* Check that the values read are the same as the values written */
    pw = data_w;
    pr = data_r;
    for (i = 0; i < CORE_DSET_DIM1; i++)
        for (j = 0; j < CORE_DSET_DIM2; j++)
            if (*pr++ != *pw++) {
                H5_FAILED();
                HDprintf("    Read different values than written in data set.\n");
                HDprintf("    At index %d,%d\n", i, j);
                TEST_ERROR;
            } /* end if */

    /* Close everything except the dataspace ID (needed below)*/
    if (H5Dclose(did) < 0)
        TEST_ERROR;
    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    /* Reopen the file and ensure that the dataset does not exist */
    if ((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl_id)) < 0)
        TEST_ERROR;
    status = H5Lexists(fid, CORE_DSET_NAME, H5P_DEFAULT);
    if (status < 0)
        TEST_ERROR;
    if (status > 0)
        FAIL_PUTS_ERROR("core VFD dataset created in file when backing store disabled");

    /* Close the file */
    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    /************************************************************************
     * Make changes to the file with the backing store flag ON to ensure
     * that they ARE propagated.
     ************************************************************************/

    /* Open the file with backing store on for read and write.
     * Changes will be saved in file.
     */
    if (H5Pset_fapl_core(fapl_id, (size_t)CORE_INCREMENT, TRUE) < 0)
        TEST_ERROR;
    if ((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl_id)) < 0)
        TEST_ERROR;

    /* Create the dataset */
    if ((did = H5Dcreate2(fid, CORE_DSET_NAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) <
        0)
        TEST_ERROR;

    /* Write the data to the dataset */
    if (H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data_w) < 0)
        TEST_ERROR;

    /* Close everything and reopen */
    if (H5Dclose(did) < 0)
        TEST_ERROR;
    if (H5Fclose(fid) < 0)
        TEST_ERROR;
    if ((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl_id)) < 0)
        TEST_ERROR;
    if ((did = H5Dopen2(fid, CORE_DSET_NAME, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Read the data back from the dataset */
    HDmemset(data_r, 0, DSET1_DIM1 * DSET1_DIM2 * sizeof(int));
    if (H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data_r) < 0)
        TEST_ERROR;

    /* Check that the values read are the same as the values written */
    pw = data_w;
    pr = data_r;
    for (i = 0; i < CORE_DSET_DIM1; i++)
        for (j = 0; j < CORE_DSET_DIM2; j++)
            if (*pw++ != *pr++) {
                H5_FAILED();
                HDprintf("    Read different values than written in data set.\n");
                HDprintf("    At index %d,%d\n", i, j);
                TEST_ERROR;
            } /* end if */

    /* Check file size API.
     * There is no garantee the size of metadata in file is constant.
     * Just try to check if it's reasonable.
     *
     * TODO: Needs justification of why is this is a reasonable size.
     */
    if (H5Fget_filesize(fid, &file_size) < 0)
        TEST_ERROR;
    if (file_size < 64 * KB || file_size > 256 * KB)
        FAIL_PUTS_ERROR("suspicious file size obtained from H5Fget_filesize");

    /* Close everything */
    if (H5Sclose(sid) < 0)
        TEST_ERROR;
    if (H5Dclose(did) < 0)
        TEST_ERROR;

    HDfree(data_w);
    HDfree(data_r);

    /* Close and delete the file */
    if (H5Fclose(fid) < 0)
        TEST_ERROR;
    h5_delete_test_file(FILENAME[1], fapl_id);

    /* Close the fapl */
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Sclose(sid);
        H5Dclose(did);
        H5Pclose(fapl_id_out);
        H5Pclose(fapl_id);
        H5Fclose(fid);
    }
    H5E_END_TRY;

    if (data_w)
        HDfree(data_w);
    if (data_r)
        HDfree(data_r);

    return -1;
} /* end test_core() */

/*-------------------------------------------------------------------------
 * Function:    test_direct
 *
 * Purpose:     Tests the file handle interface for DIRECT I/O driver
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Raymond Lu
 *              Wednesday, 20 September 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_direct(void)
{
#ifdef H5_HAVE_DIRECT
    hid_t   file = -1, fapl = -1, access_fapl = -1;
    hid_t   dset1 = -1, dset2 = -1, space1 = -1, space2 = -1;
    char    filename[1024];
    int *   fhandle = NULL;
    hsize_t file_size;
    hsize_t dims1[2], dims2[1];
    size_t  mbound;
    size_t  fbsize;
    size_t  cbsize;
    void *  proto_points = NULL, *proto_check = NULL;
    int *   points = NULL, *check = NULL, *p1 = NULL, *p2 = NULL;
    int     wdata2[DSET2_DIM] = {11, 12, 13, 14};
    int     rdata2[DSET2_DIM];
    int     i, j, n;
#endif /*H5_HAVE_DIRECT*/

    TESTING("DIRECT I/O file driver");

#ifndef H5_HAVE_DIRECT
    SKIPPED();
    return 0;
#else  /*H5_HAVE_DIRECT*/

    /* Set property list and file name for Direct driver.  Set memory alignment boundary
     * and file block size to 512 which is the minimum for Linux 2.6. */
    if ((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;
    if (H5Pset_fapl_direct(fapl, MBOUNDARY, FBSIZE, CBSIZE) < 0)
        TEST_ERROR;
    h5_fixname(FILENAME[5], fapl, filename, sizeof filename);

    /* Verify the file access properties */
    if (H5Pget_fapl_direct(fapl, &mbound, &fbsize, &cbsize) < 0)
        TEST_ERROR;
    if (mbound != MBOUNDARY || fbsize != FBSIZE || cbsize != CBSIZE)
        TEST_ERROR;

    if (H5Pset_alignment(fapl, (hsize_t)THRESHOLD, (hsize_t)FBSIZE) < 0)
        TEST_ERROR;

    H5E_BEGIN_TRY
    {
        file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    }
    H5E_END_TRY;
    if (file < 0) {
        H5Pclose(fapl);
        SKIPPED();
        HDprintf("  Probably the file system doesn't support Direct I/O\n");
        return 0;
    }

    /* Retrieve the access property list... */
    if ((access_fapl = H5Fget_access_plist(file)) < 0)
        TEST_ERROR;

    /* Check that the driver is correct */
    if (H5FD_DIRECT != H5Pget_driver(access_fapl))
        TEST_ERROR;

    /* ...and close the property list */
    if (H5Pclose(access_fapl) < 0)
        TEST_ERROR;

    /* Check file handle API */
    if (H5Fget_vfd_handle(file, H5P_DEFAULT, (void **)&fhandle) < 0)
        TEST_ERROR;
    if (*fhandle < 0)
        TEST_ERROR;

    /* Check file size API */
    if (H5Fget_filesize(file, &file_size) < 0)
        TEST_ERROR;

    /* There is no guarantee of the number of metadata allocations, but it's
     * 4 currently and the size of the file should be between 3 & 4 file buffer
     * sizes..
     */
    if (file_size < (FBSIZE * 3) || file_size >= (FBSIZE * 4))
        TEST_ERROR;

    /* Allocate aligned memory for data set 1. For data set 1, everything is aligned including
     * memory address, size of data, and file address. */
    if (0 != HDposix_memalign(&proto_points, (size_t)FBSIZE, (size_t)(DSET1_DIM1 * DSET1_DIM2 * sizeof(int))))
        TEST_ERROR;
    points = proto_points;
    if (0 != HDposix_memalign(&proto_check, (size_t)FBSIZE, (size_t)(DSET1_DIM1 * DSET1_DIM2 * sizeof(int))))
        TEST_ERROR;
    check = proto_check;

    /* Initialize the dset1 */
    p1 = points;
    for (i = n = 0; i < DSET1_DIM1; i++)
        for (j = 0; j < DSET1_DIM2; j++)
            *p1++ = n++;

    /* Create the data space1 */
    dims1[0] = DSET1_DIM1;
    dims1[1] = DSET1_DIM2;
    if ((space1 = H5Screate_simple(2, dims1, NULL)) < 0)
        TEST_ERROR;

    /* Create the dset1 */
    if ((dset1 =
             H5Dcreate2(file, DSET1_NAME, H5T_NATIVE_INT, space1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Write the data to the dset1 */
    if (H5Dwrite(dset1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, points) < 0)
        TEST_ERROR;

    if (H5Dclose(dset1) < 0)
        TEST_ERROR;

    if ((dset1 = H5Dopen2(file, DSET1_NAME, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Read the data back from dset1 */
    if (H5Dread(dset1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, check) < 0)
        TEST_ERROR;

    /* Check that the values read are the same as the values written */
    p1 = points;
    p2 = check;
    for (i = 0; i < DSET1_DIM1; i++)
        for (j = 0; j < DSET1_DIM2; j++)
            if (*p1++ != *p2++) {
                H5_FAILED();
                HDprintf("    Read different values than written in data set 1.\n");
                HDprintf("    At index %d,%d\n", i, j);
                TEST_ERROR;
            } /* end if */

    /* Create the data space2. For data set 2, memory address and data size are not aligned. */
    dims2[0] = DSET2_DIM;
    if ((space2 = H5Screate_simple(1, dims2, NULL)) < 0)
        TEST_ERROR;

    /* Create the dset2 */
    if ((dset2 =
             H5Dcreate2(file, DSET2_NAME, H5T_NATIVE_INT, space2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Write the data to the dset1 */
    if (H5Dwrite(dset2, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata2) < 0)
        TEST_ERROR;

    if (H5Dclose(dset2) < 0)
        TEST_ERROR;

    if ((dset2 = H5Dopen2(file, DSET2_NAME, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Read the data back from dset1 */
    if (H5Dread(dset2, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata2) < 0)
        TEST_ERROR;

    /* Check that the values read are the same as the values written */
    for (i = 0; i < DSET2_DIM; i++)
        if (wdata2[i] != rdata2[i]) {
            H5_FAILED();
            HDprintf("    Read different values than written in data set 2.\n");
            HDprintf("    At index %d\n", i);
            TEST_ERROR;
        } /* end if */

    if (H5Sclose(space1) < 0)
        TEST_ERROR;
    if (H5Dclose(dset1) < 0)
        TEST_ERROR;
    if (H5Sclose(space2) < 0)
        TEST_ERROR;
    if (H5Dclose(dset2) < 0)
        TEST_ERROR;

    HDfree(points);
    HDfree(check);

    /* Close and delete the file */
    if (H5Fclose(file) < 0)
        TEST_ERROR;
    h5_delete_test_file(FILENAME[5], fapl);

    /* Close the fapl */
    if (H5Pclose(fapl) < 0)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(fapl);
        H5Sclose(space1);
        H5Dclose(dset1);
        H5Sclose(space2);
        H5Dclose(dset2);
        H5Fclose(file);
    }
    H5E_END_TRY;

    if (proto_points)
        HDfree(proto_points);
    if (proto_check)
        HDfree(proto_check);

    return -1;
#endif /*H5_HAVE_DIRECT*/
}

/*-------------------------------------------------------------------------
 * Function:    test_family_opens
 *
 * Purpose:     Private function for test_family() to tests wrong ways of
 *              reopening family file.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Raymond Lu
 *              Thursday, May 19, 2005
 *
 *-------------------------------------------------------------------------
 */
/* Disable warning for "format not a string literal" here -QAK */
/*
 *      This pragma only needs to surround the snprintf() calls with
 *      'first_name' in the code below, but early (4.4.7, at least) gcc only
 *      allows diagnostic pragmas to be toggled outside of functions.
 */
H5_GCC_DIAG_OFF("format-nonliteral")
static herr_t
test_family_opens(char *fname, hid_t fa_pl)
{
    hid_t file = -1;
    char  first_name[1024];
    char  wrong_name[1024];
    int   i;

    /* Case 1: reopen file with 1st member file name and default property list */
    HDsnprintf(first_name, sizeof(first_name), fname, 0);

    H5E_BEGIN_TRY
    {
        file = H5Fopen(first_name, H5F_ACC_RDWR, H5P_DEFAULT);
    }
    H5E_END_TRY;
    if (file >= 0)
        TEST_ERROR

    /* Case 2: reopen file with correct name template but default property list */
    H5E_BEGIN_TRY
    {
        file = H5Fopen(fname, H5F_ACC_RDWR, H5P_DEFAULT);
    }
    H5E_END_TRY;
    if (file >= 0)
        TEST_ERROR

    /* Case 3: reopen file with wrong member size */
    if (H5Pset_fapl_family(fa_pl, (hsize_t)128, H5P_DEFAULT) < 0)
        TEST_ERROR;

    H5E_BEGIN_TRY
    {
        file = H5Fopen(fname, H5F_ACC_RDWR, fa_pl);
    }
    H5E_END_TRY;
    if (file >= 0)
        TEST_ERROR

    /* Case 4: reopen file with wrong name template */
    HDstrcpy(wrong_name, fname);
    for (i = 0; i < 1024; i++)
        if (wrong_name[i] == '5') {
            wrong_name[i] = '4';
            break;
        }

    if (H5Pset_fapl_family(fa_pl, (hsize_t)FAMILY_SIZE, H5P_DEFAULT) < 0)
        TEST_ERROR;

    H5E_BEGIN_TRY
    {
        file = H5Fopen(wrong_name, H5F_ACC_RDWR, fa_pl);
    }
    H5E_END_TRY;
    if (file >= 0)
        TEST_ERROR

    return 0;

error:
    return -1;
} /* end test_family_opens() */
H5_GCC_DIAG_ON("format-nonliteral")

/*-------------------------------------------------------------------------
 * Function:    test_family
 *
 * Purpose:     Tests the file handle interface for FAMILY driver
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Raymond Lu
 *              Tuesday, Sept 24, 2002
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_family(void)
{
    hid_t         file = -1, fapl = -1, fapl2 = -1, space = -1, dset = -1;
    hid_t         access_fapl  = -1;
    hid_t         driver_id    = -1; /* ID for this VFD              */
    unsigned long driver_flags = 0;  /* VFD feature flags            */
    char          filename[1024];
    char          dname[] = "dataset";
    unsigned int  i, j;
    int *         fhandle = NULL, *fhandle2 = NULL;
    int **        buf      = NULL;
    int *         buf_data = NULL;
    hsize_t       dims[2]  = {FAMILY_NUMBER, FAMILY_SIZE};
    hsize_t       file_size;

    TESTING("FAMILY file driver");

    /* Set up data array */
    if (NULL == (buf_data = (int *)HDcalloc(FAMILY_NUMBER * FAMILY_SIZE, sizeof(int))))
        TEST_ERROR;
    if (NULL == (buf = (int **)HDcalloc(FAMILY_NUMBER, sizeof(buf_data))))
        TEST_ERROR;
    for (i = 0; i < FAMILY_NUMBER; i++)
        buf[i] = buf_data + (i * FAMILY_SIZE);

    /* Set property list and file name for FAMILY driver */
    if ((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;
    if (H5Pset_fapl_family(fapl, (hsize_t)FAMILY_SIZE, H5P_DEFAULT) < 0)
        TEST_ERROR;
    h5_fixname(FILENAME[2], fapl, filename, sizeof(filename));

    /* Check that the VFD feature flags are correct */
    if ((driver_id = H5Pget_driver(fapl)) < 0)
        TEST_ERROR
    if (H5FDdriver_query(driver_id, &driver_flags) < 0)
        TEST_ERROR
    if (!(driver_flags & H5FD_FEAT_AGGREGATE_METADATA))
        TEST_ERROR
    if (!(driver_flags & H5FD_FEAT_ACCUMULATE_METADATA))
        TEST_ERROR
    if (!(driver_flags & H5FD_FEAT_DATA_SIEVE))
        TEST_ERROR
    if (!(driver_flags & H5FD_FEAT_AGGREGATE_SMALLDATA))
        TEST_ERROR
    /* Check for extra flags not accounted for above */
    if (driver_flags != (H5FD_FEAT_AGGREGATE_METADATA | H5FD_FEAT_ACCUMULATE_METADATA | H5FD_FEAT_DATA_SIEVE |
                         H5FD_FEAT_AGGREGATE_SMALLDATA))
        TEST_ERROR

    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;

    if (H5Fclose(file) < 0)
        TEST_ERROR;

    /* Test different wrong ways to reopen family files where there's only
     * one member file existing. */
    if (test_family_opens(filename, fapl) < 0)
        TEST_ERROR;

    /* Reopen the file with default member file size */
    if (H5Pset_fapl_family(fapl, (hsize_t)H5F_FAMILY_DEFAULT, H5P_DEFAULT) < 0)
        TEST_ERROR;

    if ((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR;

    /* Check file size API */
    if (H5Fget_filesize(file, &file_size) < 0)
        TEST_ERROR;

    /* The file size is supposed to be about 800 bytes right now. */
    if (file_size < (KB / 2) || file_size > KB)
        TEST_ERROR;

    /* Create and write dataset */
    if ((space = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;

    /* Retrieve the access property list... */
    if ((access_fapl = H5Fget_access_plist(file)) < 0)
        TEST_ERROR;

    /* Check that the driver is correct */
    if (H5FD_FAMILY != H5Pget_driver(access_fapl))
        TEST_ERROR;

    /* ...and close the property list */
    if (H5Pclose(access_fapl) < 0)
        TEST_ERROR;

    if ((dset = H5Dcreate2(file, dname, H5T_NATIVE_INT, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    for (i = 0; i < FAMILY_NUMBER; i++)
        for (j = 0; j < FAMILY_SIZE; j++)
            buf[i][j] = (int)((i * 10000) + j);

    if (H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_data) < 0)
        TEST_ERROR;

    /* check file handle API */
    if ((fapl2 = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;
    if (H5Pset_family_offset(fapl2, (hsize_t)0) < 0)
        TEST_ERROR;

    if (H5Fget_vfd_handle(file, fapl2, (void **)&fhandle) < 0)
        TEST_ERROR;
    if (*fhandle < 0)
        TEST_ERROR;

    if (H5Pset_family_offset(fapl2, (hsize_t)(FAMILY_SIZE * 2)) < 0)
        TEST_ERROR;
    if (H5Fget_vfd_handle(file, fapl2, (void **)&fhandle2) < 0)
        TEST_ERROR;
    if (*fhandle2 < 0)
        TEST_ERROR;

    /* Check file size API */
    if (H5Fget_filesize(file, &file_size) < 0)
        TEST_ERROR;

        /* Some data has been written.  The file size should be bigger (18KB+976)
         * bytes if int size is 4 bytes) now. */
#if H5_SIZEOF_INT <= 4
    if (file_size < (18 * KB) || file_size > (20 * KB))
        TEST_ERROR;
#elif H5_SIZEOF_INT >= 8
    if (file_size < (32 * KB) || file_size > (40 * KB))
        TEST_ERROR;
#endif

    if (H5Sclose(space) < 0)
        TEST_ERROR;
    if (H5Dclose(dset) < 0)
        TEST_ERROR;
    if (H5Pclose(fapl2) < 0)
        TEST_ERROR;
    if (H5Fclose(file) < 0)
        TEST_ERROR;

    /* Test different wrong ways to reopen family files when there're multiple
     * member files existing. */
    if (test_family_opens(filename, fapl) < 0)
        TEST_ERROR;

    /* Reopen the file with correct member file size. */
    if (H5Pset_fapl_family(fapl, (hsize_t)FAMILY_SIZE, H5P_DEFAULT) < 0)
        TEST_ERROR;

    if ((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR;

    /* Close and delete the file */
    if (H5Fclose(file) < 0)
        TEST_ERROR;
    h5_delete_test_file(FILENAME[2], fapl);

    /* Close the fapl */
    if (H5Pclose(fapl) < 0)
        TEST_ERROR;

    HDfree(buf);
    HDfree(buf_data);

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Sclose(space);
        H5Dclose(dset);
        H5Pclose(fapl);
        H5Pclose(fapl2);
        H5Fclose(file);
    }
    H5E_END_TRY;

    HDfree(buf);
    HDfree(buf_data);

    return FAIL;
} /* end test_family() */

/*-------------------------------------------------------------------------
 * Function:    test_family_compat
 *
 * Purpose:     Tests the backward compatibility for FAMILY driver.
 *              See if we can open files created with v1.6 library.
 *              The source file was created by the test/file_handle.c
 *              of the v1.6 library.  Then tools/misc/h5repart.c was
 *              used to concantenated.  The command was "h5repart -m 5k
 *              family_file%05d.h5 family_v16_%05d.h5".
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Raymond Lu
 *              June 3, 2005
 *
 *-------------------------------------------------------------------------
 */
/* Disable warning for "format not a string literal" here -QAK */
/*
 *      This pragma only needs to surround the snprintf() calls with
 *      'newname_individual', etc. in the code below, but early (4.4.7, at least) gcc only
 *      allows diagnostic pragmas to be toggled outside of functions.
 */
H5_GCC_DIAG_OFF("format-nonliteral")
static herr_t
test_family_compat(void)
{
    hid_t file = -1, fapl = -1;
    hid_t dset    = -1;
    char  dname[] = "dataset";
    char  filename[1024];
    char  pathname[1024], pathname_individual[1024];
    char  newname[1024], newname_individual[1024];
    int   counter = 0;

    TESTING("FAMILY file driver backward compatibility");

    /* Set property list and file name for FAMILY driver */
    if ((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;
    if (H5Pset_fapl_family(fapl, (hsize_t)FAMILY_SIZE2, H5P_DEFAULT) < 0)
        TEST_ERROR;

    h5_fixname(COMPAT_BASENAME, fapl, filename, sizeof(filename));
    h5_fixname(FILENAME[3], fapl, newname, sizeof(newname));

    pathname[0] = '\0';
    HDstrcat(pathname, filename);

    /* The following code makes the copies of the family files in the source directory.
     * Since we're going to open the files with write mode, this protects the original
     * files.
     */
    HDsnprintf(newname_individual, sizeof(newname_individual), newname, counter);
    HDsnprintf(pathname_individual, sizeof(pathname_individual), pathname, counter);

    while (h5_make_local_copy(pathname_individual, newname_individual) >= 0) {
        counter++;
        HDsnprintf(newname_individual, sizeof(newname_individual), newname, counter);
        HDsnprintf(pathname_individual, sizeof(pathname_individual), pathname, counter);
    } /* end while */

    /* Make sure we can open the file.  Use the read and write mode to flush the
     * superblock. */
    if ((file = H5Fopen(newname, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR;

    if ((dset = H5Dopen2(file, dname, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if (H5Dclose(dset) < 0)
        TEST_ERROR;

    if (H5Fclose(file) < 0)
        TEST_ERROR;

    /* Open the file again to make sure it isn't corrupted. */
    if ((file = H5Fopen(newname, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR;

    if ((dset = H5Dopen2(file, dname, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if (H5Dclose(dset) < 0)
        TEST_ERROR;

    /* Close and delete the file */
    if (H5Fclose(file) < 0)
        TEST_ERROR;
    h5_delete_test_file(FILENAME[3], fapl);

    /* Close the fapl */
    if (H5Pclose(fapl) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Fclose(file);
        H5Pclose(fapl);
    }
    H5E_END_TRY;

    return -1;
} /* end test_family_compat() */
H5_GCC_DIAG_ON("format-nonliteral")

/*-------------------------------------------------------------------------
 * Function:    test_family_member_fapl
 *
 * Purpose:     Actually use the member fapl input to the member vfd.
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Jacob Smith
 *              21 May 2019
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_family_member_fapl(void)
{
    hid_t    file         = H5I_INVALID_HID;
    hid_t    fapl_id      = H5I_INVALID_HID;
    hid_t    memb_fapl_id = H5I_INVALID_HID;
    hid_t    space        = H5I_INVALID_HID;
    hid_t    dset         = H5I_INVALID_HID;
    char     filename[1024];
    char     dname[]  = "dataset";
    unsigned i        = 0;
    unsigned j        = 0;
    int **   buf      = NULL;
    int *    buf_data = NULL;
    hsize_t  dims[2]  = {FAMILY_NUMBER, FAMILY_SIZE};

    TESTING("Family member FAPL");

    /* Set up data array */
    if (NULL == (buf_data = (int *)HDcalloc(FAMILY_NUMBER * FAMILY_SIZE, sizeof(int))))
        TEST_ERROR;
    if (NULL == (buf = (int **)HDcalloc(FAMILY_NUMBER, sizeof(buf_data))))
        TEST_ERROR;
    for (i = 0; i < FAMILY_NUMBER; i++)
        buf[i] = buf_data + (i * FAMILY_SIZE);

    if ((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) == H5I_INVALID_HID)
        TEST_ERROR;

    if ((memb_fapl_id = H5Pcreate(H5P_FILE_ACCESS)) == H5I_INVALID_HID)
        TEST_ERROR;

    if (H5Pset_fapl_sec2(memb_fapl_id) == FAIL)
        TEST_ERROR;
    if (H5Pset_fapl_family(fapl_id, (hsize_t)FAMILY_SIZE, memb_fapl_id) == FAIL)
        TEST_ERROR;

    h5_fixname(FILENAME[2], fapl_id, filename, sizeof(filename));

    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id)) == H5I_INVALID_HID)
        TEST_ERROR;

    if ((space = H5Screate_simple(2, dims, NULL)) == H5I_INVALID_HID)
        TEST_ERROR;

    /* Create and write to dataset, then close file.
     */
    if ((dset = H5Dcreate2(file, dname, H5T_NATIVE_INT, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) ==
        H5I_INVALID_HID)
        TEST_ERROR;

    for (i = 0; i < FAMILY_NUMBER; i++) {
        for (j = 0; j < FAMILY_SIZE; j++) {
            buf[i][j] = (int)((i * 10000) + j);
        }
    }

    if (H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_data) == FAIL)
        TEST_ERROR;

    if (H5Dclose(dset) == FAIL)
        TEST_ERROR;
    if (H5Sclose(space) == FAIL)
        TEST_ERROR;
    if (H5Fclose(file) == FAIL)
        TEST_ERROR;

    /* "Close" member FAPL at top level and re-open file.
     * Should succeed, with library managing reference count properly
     */
    if (H5Pclose(memb_fapl_id) == FAIL)
        TEST_ERROR;

    if ((file = H5Fopen(filename, H5F_ACC_RDWR, fapl_id)) == H5I_INVALID_HID)
        TEST_ERROR;

    if (H5Fclose(file) == FAIL)
        TEST_ERROR;

    h5_delete_test_file(FILENAME[2], fapl_id);

    if (H5Pclose(fapl_id) == FAIL)
        TEST_ERROR;

    HDfree(buf);
    HDfree(buf_data);

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Sclose(space);
        H5Dclose(dset);
        H5Pclose(memb_fapl_id);
        H5Pclose(fapl_id);
        H5Fclose(file);
    }
    H5E_END_TRY;

    HDfree(buf);
    HDfree(buf_data);

    return FAIL;
} /* end test_family_member_fapl() */

/*-------------------------------------------------------------------------
 * Function:    test_multi_opens
 *
 * Purpose:     Private function for test_multi() to tests wrong ways of
 *              reopening multi file.
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Raymond Lu
 *              Thursday, May 19, 2005
 *
 *-------------------------------------------------------------------------
 */
/* Disable warning for "format not a string literal" here -QAK */
/*
 *      This pragma only needs to surround the snprintf() calls with
 *      'sf_name' in the code below, but early (4.4.7, at least) gcc only
 *      allows diagnostic pragmas to be toggled outside of functions.
 */
H5_GCC_DIAG_OFF("format-nonliteral")
static herr_t
test_multi_opens(char *fname)
{
    hid_t fid = H5I_INVALID_HID;
    char  super_name[1024]; /*name string "%%s-s.h5"*/
    char  sf_name[1024];    /*name string "multi_file-s.h5"*/

    /* Case: reopen with the name of super file and default property list */
    HDsnprintf(super_name, sizeof(super_name), "%%s-%c.h5", 's');
    HDsnprintf(sf_name, sizeof(sf_name), super_name, fname);

    H5E_BEGIN_TRY
    {
        fid = H5Fopen(sf_name, H5F_ACC_RDWR, H5P_DEFAULT);
    }
    H5E_END_TRY;

    return (fid >= 0 ? FAIL : SUCCEED);
} /* end test_multi_opens() */
H5_GCC_DIAG_ON("format-nonliteral")

/*-------------------------------------------------------------------------
 * Function:    test_multi
 *
 * Purpose:     Tests the file handle interface for MUTLI driver
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Raymond Lu
 *              Tuesday, Sept 24, 2002
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_multi(void)
{
    hid_t         file = -1, fapl = -1, fapl2 = -1, dset = -1, space = -1;
    hid_t         root = -1, attr = -1, aspace = -1, atype = -1;
    hid_t         access_fapl  = -1;
    hid_t         driver_id    = -1; /* ID for this VFD              */
    unsigned long driver_flags = 0;  /* VFD feature flags            */
    char          filename[1024];
    int *         fhandle2 = NULL, *fhandle = NULL;
    hsize_t       file_size;
    H5FD_mem_t    mt, memb_map[H5FD_MEM_NTYPES];
    hid_t         memb_fapl[H5FD_MEM_NTYPES];
    haddr_t       memb_addr[H5FD_MEM_NTYPES];
    const char *  memb_name[H5FD_MEM_NTYPES];
    char          sv[H5FD_MEM_NTYPES][32];
    hsize_t       dims[2]  = {MULTI_SIZE, MULTI_SIZE};
    hsize_t       adims[1] = {1};
    char          dname[]  = "dataset";
    char          meta[]   = "this is some metadata on this file";
    int           i, j;
    int **        buf      = NULL;
    int *         buf_data = NULL;

    TESTING("MULTI file driver");

    /* Set up data array */
    if (NULL == (buf_data = (int *)HDcalloc(MULTI_SIZE * MULTI_SIZE, sizeof(int))))
        TEST_ERROR;
    if (NULL == (buf = (int **)HDcalloc(MULTI_SIZE, sizeof(buf_data))))
        TEST_ERROR;
    for (i = 0; i < MULTI_SIZE; i++)
        buf[i] = buf_data + (i * MULTI_SIZE);

    /* Set file access property list for MULTI driver */
    if ((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;

    HDmemset(memb_map, 0, sizeof(memb_map));
    HDmemset(memb_fapl, 0, sizeof(memb_fapl));
    HDmemset(memb_name, 0, sizeof(memb_name));
    HDmemset(memb_addr, 0, sizeof(memb_addr));
    HDmemset(sv, 0, sizeof(sv));

    for (mt = H5FD_MEM_DEFAULT; mt < H5FD_MEM_NTYPES; mt++) {
        memb_fapl[mt] = H5P_DEFAULT;
        memb_map[mt]  = H5FD_MEM_SUPER;
    }
    memb_map[H5FD_MEM_DRAW]  = H5FD_MEM_DRAW;
    memb_map[H5FD_MEM_BTREE] = H5FD_MEM_BTREE;
    memb_map[H5FD_MEM_GHEAP] = H5FD_MEM_GHEAP;

    HDsprintf(sv[H5FD_MEM_SUPER], "%%s-%c.h5", 's');
    memb_name[H5FD_MEM_SUPER] = sv[H5FD_MEM_SUPER];
    memb_addr[H5FD_MEM_SUPER] = 0;

    HDsprintf(sv[H5FD_MEM_BTREE], "%%s-%c.h5", 'b');
    memb_name[H5FD_MEM_BTREE] = sv[H5FD_MEM_BTREE];
    memb_addr[H5FD_MEM_BTREE] = HADDR_MAX / 4;

    HDsprintf(sv[H5FD_MEM_DRAW], "%%s-%c.h5", 'r');
    memb_name[H5FD_MEM_DRAW] = sv[H5FD_MEM_DRAW];
    memb_addr[H5FD_MEM_DRAW] = HADDR_MAX / 2;

    HDsprintf(sv[H5FD_MEM_GHEAP], "%%s-%c.h5", 'g');
    memb_name[H5FD_MEM_GHEAP] = sv[H5FD_MEM_GHEAP];
    memb_addr[H5FD_MEM_GHEAP] = (HADDR_MAX / 4) * 3;

    if (H5Pset_fapl_multi(fapl, memb_map, memb_fapl, memb_name, memb_addr, TRUE) < 0)
        TEST_ERROR;
    h5_fixname(FILENAME[4], fapl, filename, sizeof filename);

    /* Check that the VFD feature flags are correct */
    if ((driver_id = H5Pget_driver(fapl)) < 0)
        TEST_ERROR
    if (H5FDdriver_query(driver_id, &driver_flags) < 0)
        TEST_ERROR
    if (!(driver_flags & H5FD_FEAT_DATA_SIEVE))
        TEST_ERROR
    if (!(driver_flags & H5FD_FEAT_AGGREGATE_SMALLDATA))
        TEST_ERROR
    if (!(driver_flags & H5FD_FEAT_USE_ALLOC_SIZE))
        TEST_ERROR
    if (!(driver_flags & H5FD_FEAT_PAGED_AGGR))
        TEST_ERROR
    /* Check for extra flags not accounted for above */
    if (driver_flags != (H5FD_FEAT_DATA_SIEVE | H5FD_FEAT_AGGREGATE_SMALLDATA | H5FD_FEAT_USE_ALLOC_SIZE |
                         H5FD_FEAT_PAGED_AGGR))
        TEST_ERROR

    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;

    if (H5Fclose(file) < 0)
        TEST_ERROR;

    /* Test wrong ways to reopen multi files */
    if (test_multi_opens(filename) < 0)
        TEST_ERROR;

    /* Reopen the file */
    if ((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR;

    /* Create and write data set */
    if ((space = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;

    /* Retrieve the access property list... */
    if ((access_fapl = H5Fget_access_plist(file)) < 0)
        TEST_ERROR;

    /* Check that the driver is correct */
    if (H5FD_MULTI != H5Pget_driver(access_fapl))
        TEST_ERROR;

    /* ...and close the property list */
    if (H5Pclose(access_fapl) < 0)
        TEST_ERROR;

    /* Check file size API */
    if (H5Fget_filesize(file, &file_size) < 0)
        TEST_ERROR;

    /* Before any data is written, the raw data file is empty.  So
     * the file size is only the size of b-tree + HADDR_MAX/4.
     */
    if (file_size < HADDR_MAX / 4 || file_size > HADDR_MAX / 2)
        TEST_ERROR;

    if ((dset = H5Dcreate2(file, dname, H5T_NATIVE_INT, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    for (i = 0; i < MULTI_SIZE; i++)
        for (j = 0; j < MULTI_SIZE; j++)
            buf[i][j] = i * 10000 + j;
    if (H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_data) < 0)
        TEST_ERROR;

    if ((fapl2 = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;
    if (H5Pset_multi_type(fapl2, H5FD_MEM_SUPER) < 0)
        TEST_ERROR;
    if (H5Fget_vfd_handle(file, fapl2, (void **)&fhandle) < 0)
        TEST_ERROR;
    if (*fhandle < 0)
        TEST_ERROR;

    if (H5Pset_multi_type(fapl2, H5FD_MEM_DRAW) < 0)
        TEST_ERROR;
    if (H5Fget_vfd_handle(file, fapl2, (void **)&fhandle2) < 0)
        TEST_ERROR;
    if (*fhandle2 < 0)
        TEST_ERROR;

    /* Check file size API */
    if (H5Fget_filesize(file, &file_size) < 0)
        TEST_ERROR;

    /* After the data is written, the file size is huge because the
     * beginning of raw data file is set at HADDR_MAX/2.  It's supposed
     * to be (HADDR_MAX/2 + 128*128*4)
     */
    if (file_size < HADDR_MAX / 2 || file_size > HADDR_MAX)
        TEST_ERROR;

    if (H5Sclose(space) < 0)
        TEST_ERROR;
    if (H5Dclose(dset) < 0)
        TEST_ERROR;
    if (H5Pclose(fapl2) < 0)
        TEST_ERROR;

    /* Create and write attribute for the root group. */
    if ((root = H5Gopen2(file, "/", H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Attribute string. */
    if ((atype = H5Tcopy(H5T_C_S1)) < 0)
        TEST_ERROR;

    if (H5Tset_size(atype, HDstrlen(meta) + 1) < 0)
        TEST_ERROR;

    if (H5Tset_strpad(atype, H5T_STR_NULLTERM) < 0)
        TEST_ERROR;

    /* Create and write attribute */
    if ((aspace = H5Screate_simple(1, adims, NULL)) < 0)
        TEST_ERROR;

    if ((attr = H5Acreate2(root, "Metadata", atype, aspace, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if (H5Awrite(attr, atype, meta) < 0)
        TEST_ERROR;

    /* Close IDs */
    if (H5Tclose(atype) < 0)
        TEST_ERROR;
    if (H5Sclose(aspace) < 0)
        TEST_ERROR;
    if (H5Aclose(attr) < 0)
        TEST_ERROR;

    /* Close and delete the file */
    if (H5Fclose(file) < 0)
        TEST_ERROR;
    h5_delete_test_file(FILENAME[4], fapl);

    /* Close the fapl */
    if (H5Pclose(fapl) < 0)
        TEST_ERROR;

    HDfree(buf);
    HDfree(buf_data);

    PASSED();

    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Sclose(space);
        H5Dclose(dset);
        H5Pclose(fapl);
        H5Pclose(fapl2);
        H5Fclose(file);
        H5Aclose(attr);
    }
    H5E_END_TRY;

    HDfree(buf);
    HDfree(buf_data);

    return FAIL;
} /* end test_multi() */

/*-------------------------------------------------------------------------
 * Function:    test_multi_compat
 *
 * Purpose:     Tests the backward compatibility for MULTI driver.
 *              See if we can open files created with v1.6 library.
 *              The source file was created by the test/file_handle.c
 *              of the v1.6 library.  This test verifies the fix for
 *              Issue 2598. In v1.6 library, there was EOA for the whole
 *              MULTI file saved in the super block.  We took it out in
 *              v1.8 library because it's meaningless for the MULTI file.
 *              v1.8 library saves the EOA for the metadata file, instead.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Raymond Lu
 *              21 June 2011
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_multi_compat(void)
{
    hid_t       file = -1, fapl = -1, dset = -1, space = -1;
    char        newname[1024];
    char        filename_s[1024], newname_s[1024];
    char        filename_r[1024], newname_r[1024];
    H5FD_mem_t  mt, memb_map[H5FD_MEM_NTYPES];
    hid_t       memb_fapl[H5FD_MEM_NTYPES];
    haddr_t     memb_addr[H5FD_MEM_NTYPES];
    const char *memb_name[H5FD_MEM_NTYPES];
    char        sv[H5FD_MEM_NTYPES][32];
    hsize_t     dims[2] = {MULTI_SIZE, MULTI_SIZE};
    int         i, j;
    int **      buf      = NULL;
    int *       buf_data = NULL;

    TESTING("MULTI file driver backward compatibility");

    /* Set up data array */
    if (NULL == (buf_data = (int *)HDcalloc(MULTI_SIZE * MULTI_SIZE, sizeof(int))))
        TEST_ERROR;
    if (NULL == (buf = (int **)HDcalloc(MULTI_SIZE, sizeof(buf_data))))
        TEST_ERROR;
    for (i = 0; i < MULTI_SIZE; i++)
        buf[i] = buf_data + (i * MULTI_SIZE);

    /* Set file access property list for MULTI driver */
    if ((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;

    HDmemset(memb_map, 0, sizeof memb_map);
    HDmemset(memb_fapl, 0, sizeof memb_fapl);
    HDmemset(memb_name, 0, sizeof memb_name);
    HDmemset(memb_addr, 0, sizeof memb_addr);
    HDmemset(sv, 0, sizeof sv);

    for (mt = H5FD_MEM_DEFAULT; mt < H5FD_MEM_NTYPES; mt++)
        memb_map[mt] = H5FD_MEM_SUPER;
    memb_map[H5FD_MEM_DRAW] = H5FD_MEM_DRAW;

    memb_fapl[H5FD_MEM_SUPER] = H5P_DEFAULT;
    HDsprintf(sv[H5FD_MEM_SUPER], "%%s-%c.h5", 's');
    memb_name[H5FD_MEM_SUPER] = sv[H5FD_MEM_SUPER];
    memb_addr[H5FD_MEM_SUPER] = 0;

    memb_fapl[H5FD_MEM_DRAW] = H5P_DEFAULT;
    HDsprintf(sv[H5FD_MEM_DRAW], "%%s-%c.h5", 'r');
    memb_name[H5FD_MEM_DRAW] = sv[H5FD_MEM_DRAW];
    memb_addr[H5FD_MEM_DRAW] = HADDR_MAX / 2;

    if (H5Pset_fapl_multi(fapl, memb_map, memb_fapl, memb_name, memb_addr, TRUE) < 0)
        TEST_ERROR;

    h5_fixname(FILENAME[9], fapl, newname, sizeof newname);

    /* Make copy for the data file in the build directory, to protect the
     * original file in the source directory
     */
    HDsprintf(filename_s, "%s-%c.h5", MULTI_COMPAT_BASENAME, 's');
    HDsprintf(newname_s, "%s-%c.h5", FILENAME[9], 's');
    h5_make_local_copy(filename_s, newname_s);

    HDsprintf(filename_r, "%s-%c.h5", MULTI_COMPAT_BASENAME, 'r');
    HDsprintf(newname_r, "%s-%c.h5", FILENAME[9], 'r');
    h5_make_local_copy(filename_r, newname_r);

    /* Reopen the file for read only.  Verify 1.8 library can open file
     * created with 1.6 library.
     */
    if ((file = H5Fopen(newname, H5F_ACC_RDONLY, fapl)) < 0)
        TEST_ERROR;

    if ((dset = H5Dopen2(file, DSET1_NAME, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if (H5Dclose(dset) < 0)
        TEST_ERROR;

    if (H5Fclose(file) < 0)
        TEST_ERROR;

    /* Make sure we can reopen the file for read and write */
    if ((file = H5Fopen(newname, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR;

    if ((dset = H5Dopen2(file, DSET1_NAME, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if (H5Dclose(dset) < 0)
        TEST_ERROR;

    if (H5Fclose(file) < 0)
        TEST_ERROR;

    /* Reopen the file for adding another dataset. The new EOA for metadata file
     * should be written to the file */
    if ((file = H5Fopen(newname, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR;

    /* Create and write data set */
    if ((space = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;

    if ((dset = H5Dcreate2(file, DSET3_NAME, H5T_NATIVE_INT, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) <
        0)
        TEST_ERROR;

    for (i = 0; i < MULTI_SIZE; i++)
        for (j = 0; j < MULTI_SIZE; j++)
            buf[i][j] = i * 10000 + j;
    if (H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_data) < 0)
        TEST_ERROR;

    if (H5Dclose(dset) < 0)
        TEST_ERROR;

    if (H5Sclose(space) < 0)
        TEST_ERROR;

    if (H5Fclose(file) < 0)
        TEST_ERROR;

    /* Reopen the file for read only again. Verify the library can handle
     * the EOA correctly */
    if ((file = H5Fopen(newname, H5F_ACC_RDONLY, fapl)) < 0)
        TEST_ERROR;

    if ((dset = H5Dopen2(file, DSET1_NAME, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if (H5Dclose(dset) < 0)
        TEST_ERROR;

    if ((dset = H5Dopen2(file, DSET3_NAME, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if (H5Dclose(dset) < 0)
        TEST_ERROR;

    /* Close and delete the file */
    if (H5Fclose(file) < 0)
        TEST_ERROR;
    h5_delete_test_file(FILENAME[9], fapl);

    /* Close the fapl */
    if (H5Pclose(fapl) < 0)
        TEST_ERROR;

    HDfree(buf);
    HDfree(buf_data);

    PASSED();

    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Sclose(space);
        H5Dclose(dset);
        H5Pclose(fapl);
        H5Fclose(file);
    }
    H5E_END_TRY;

    HDfree(buf);
    HDfree(buf_data);

    return FAIL;
} /* end test_multi_compat() */

/*-------------------------------------------------------------------------
 * Function:    test_log
 *
 * Purpose:     Tests the file handle interface for log driver
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Dana Robinson
 *              Tuesday, March 22, 2011
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_log(void)
{
    hid_t         file         = -1;
    hid_t         fapl         = -1;
    hid_t         access_fapl  = -1;
    hid_t         driver_id    = -1; /* ID for this VFD              */
    unsigned long driver_flags = 0;  /* VFD feature flags            */
    char          filename[1024];
    int *         fhandle   = NULL;
    hsize_t       file_size = 0;
    unsigned int  flags     = H5FD_LOG_ALL;
    size_t        buf_size  = 4 * KB;

    TESTING("LOG file driver");

    /* Set property list and file name for log driver. */
    if ((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;
    if (H5Pset_fapl_log(fapl, LOG_FILENAME, flags, buf_size) < 0)
        TEST_ERROR;
    h5_fixname(FILENAME[6], fapl, filename, sizeof filename);

    /* Check that the VFD feature flags are correct */
    if ((driver_id = H5Pget_driver(fapl)) < 0)
        TEST_ERROR
    if (H5FDdriver_query(driver_id, &driver_flags) < 0)
        TEST_ERROR
    if (!(driver_flags & H5FD_FEAT_AGGREGATE_METADATA))
        TEST_ERROR
    if (!(driver_flags & H5FD_FEAT_ACCUMULATE_METADATA))
        TEST_ERROR
    if (!(driver_flags & H5FD_FEAT_DATA_SIEVE))
        TEST_ERROR
    if (!(driver_flags & H5FD_FEAT_AGGREGATE_SMALLDATA))
        TEST_ERROR
    if (!(driver_flags & H5FD_FEAT_POSIX_COMPAT_HANDLE))
        TEST_ERROR
    if (!(driver_flags & H5FD_FEAT_SUPPORTS_SWMR_IO))
        TEST_ERROR
    if (!(driver_flags & H5FD_FEAT_DEFAULT_VFD_COMPATIBLE))
        TEST_ERROR
    /* Check for extra flags not accounted for above */
    if (driver_flags != (H5FD_FEAT_AGGREGATE_METADATA | H5FD_FEAT_ACCUMULATE_METADATA | H5FD_FEAT_DATA_SIEVE |
                         H5FD_FEAT_AGGREGATE_SMALLDATA | H5FD_FEAT_POSIX_COMPAT_HANDLE |
                         H5FD_FEAT_SUPPORTS_SWMR_IO | H5FD_FEAT_DEFAULT_VFD_COMPATIBLE))
        TEST_ERROR

    /* Create the test file */
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;

    /* Retrieve the access property list... */
    if ((access_fapl = H5Fget_access_plist(file)) < 0)
        TEST_ERROR;

    /* Check that the driver is correct */
    if (H5FD_LOG != H5Pget_driver(access_fapl))
        TEST_ERROR;

    /* ...and close the property list */
    if (H5Pclose(access_fapl) < 0)
        TEST_ERROR;

    /* Check file handle API */
    if (H5Fget_vfd_handle(file, H5P_DEFAULT, (void **)&fhandle) < 0)
        TEST_ERROR;
    if (*fhandle < 0)
        TEST_ERROR;

    /* Check file size API */
    if (H5Fget_filesize(file, &file_size) < 0)
        TEST_ERROR;

    /* There is no guarantee the size of metadata in file is constant.
     * Just try to check if it's reasonable.  It's 2KB right now.
     */
    if (file_size < 1 * KB || file_size > 4 * KB)
        TEST_ERROR;

    /* Close and delete the file */
    if (H5Fclose(file) < 0)
        TEST_ERROR;
    h5_delete_test_file(FILENAME[6], fapl);

    /* Close the fapl */
    if (H5Pclose(fapl) < 0)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(fapl);
        H5Fclose(file);
    }
    H5E_END_TRY;
    return -1;
}

/*-------------------------------------------------------------------------
 * Function:    test_stdio
 *
 * Purpose:     Tests the file handle interface for STDIO driver
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Dana Robinson
 *              Tuesday, March 22, 2011
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_stdio(void)
{
    hid_t         file         = -1;
    hid_t         fapl         = -1;
    hid_t         access_fapl  = -1;
    hid_t         driver_id    = -1; /* ID for this VFD              */
    unsigned long driver_flags = 0;  /* VFD feature flags            */
    char          filename[1024];
    FILE *        fhandle   = NULL;
    hsize_t       file_size = 0;

    TESTING("STDIO file driver");

    /* Set property list and file name for STDIO driver. */
    if ((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;
    if (H5Pset_fapl_stdio(fapl) < 0)
        TEST_ERROR;
    h5_fixname(FILENAME[7], fapl, filename, sizeof filename);

    /* Check that the VFD feature flags are correct */
    if ((driver_id = H5Pget_driver(fapl)) < 0)
        TEST_ERROR
    if (H5FDdriver_query(driver_id, &driver_flags) < 0)
        TEST_ERROR
    if (!(driver_flags & H5FD_FEAT_AGGREGATE_METADATA))
        TEST_ERROR
    if (!(driver_flags & H5FD_FEAT_ACCUMULATE_METADATA))
        TEST_ERROR
    if (!(driver_flags & H5FD_FEAT_DATA_SIEVE))
        TEST_ERROR
    if (!(driver_flags & H5FD_FEAT_AGGREGATE_SMALLDATA))
        TEST_ERROR
    if (!(driver_flags & H5FD_FEAT_DEFAULT_VFD_COMPATIBLE))
        TEST_ERROR
    /* Check for extra flags not accounted for above */
    if (driver_flags != (H5FD_FEAT_AGGREGATE_METADATA | H5FD_FEAT_ACCUMULATE_METADATA | H5FD_FEAT_DATA_SIEVE |
                         H5FD_FEAT_AGGREGATE_SMALLDATA | H5FD_FEAT_DEFAULT_VFD_COMPATIBLE))
        TEST_ERROR

    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;

    /* Retrieve the access property list... */
    if ((access_fapl = H5Fget_access_plist(file)) < 0)
        TEST_ERROR;

    /* Check that the driver is correct */
    if (H5FD_STDIO != H5Pget_driver(access_fapl))
        TEST_ERROR;

    /* ...and close the property list */
    if (H5Pclose(access_fapl) < 0)
        TEST_ERROR;

    /* Check file handle API */
    if (H5Fget_vfd_handle(file, H5P_DEFAULT, (void **)&fhandle) < 0)
        TEST_ERROR;
    if (NULL == fhandle)
        TEST_ERROR;

    /* Check file size API */
    if (H5Fget_filesize(file, &file_size) < 0)
        TEST_ERROR;

    /* There is no guarantee the size of metadata in file is constant.
     * Just try to check if it's reasonable.  It's 2KB right now.
     */
    if (file_size < 1 * KB || file_size > 4 * KB)
        TEST_ERROR;

    /* Close and delete the file */
    if (H5Fclose(file) < 0)
        TEST_ERROR;
    h5_delete_test_file(FILENAME[7], fapl);

    /* Close the fapl */
    if (H5Pclose(fapl) < 0)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(fapl);
        H5Fclose(file);
    }
    H5E_END_TRY;
    return -1;
}

/*-------------------------------------------------------------------------
 * Function:    test_windows
 *
 * Purpose:     Tests the file handle interface for WINDOWS driver
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Dana Robinson
 *              Tuesday, March 22, 2011
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_windows(void)
{
#ifdef H5_HAVE_WINDOWS

    hid_t         file         = -1;
    hid_t         fapl         = -1;
    hid_t         access_fapl  = -1;
    hid_t         driver_id    = -1; /* ID for this VFD              */
    unsigned long driver_flags = 0;  /* VFD feature flags            */
    char          filename[1024];
    int *         fhandle   = NULL;
    hsize_t       file_size = 0;

#endif /*H5_HAVE_WINDOWS*/

    TESTING("WINDOWS file driver");

#ifndef H5_HAVE_WINDOWS

    SKIPPED();
    return 0;

#else /* H5_HAVE_WINDOWS */

    /* Set property list and file name for WINDOWS driver. */
    if ((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;
    if (H5Pset_fapl_windows(fapl) < 0)
        TEST_ERROR;
    h5_fixname(FILENAME[8], fapl, filename, sizeof filename);

    /* Check that the VFD feature flags are correct */
    if ((driver_id = H5Pget_driver(fapl)) < 0)
        TEST_ERROR
    if (H5FDdriver_query(driver_id, &driver_flags) < 0)
        TEST_ERROR
    if (!(driver_flags & H5FD_FEAT_AGGREGATE_METADATA))
        TEST_ERROR
    if (!(driver_flags & H5FD_FEAT_ACCUMULATE_METADATA))
        TEST_ERROR
    if (!(driver_flags & H5FD_FEAT_DATA_SIEVE))
        TEST_ERROR
    if (!(driver_flags & H5FD_FEAT_AGGREGATE_SMALLDATA))
        TEST_ERROR
    if (!(driver_flags & H5FD_FEAT_POSIX_COMPAT_HANDLE))
        TEST_ERROR
    if (!(driver_flags & H5FD_FEAT_SUPPORTS_SWMR_IO))
        TEST_ERROR
    if (!(driver_flags & H5FD_FEAT_DEFAULT_VFD_COMPATIBLE))
        TEST_ERROR
    /* Check for extra flags not accounted for above */
    if (driver_flags != (H5FD_FEAT_AGGREGATE_METADATA | H5FD_FEAT_ACCUMULATE_METADATA | H5FD_FEAT_DATA_SIEVE |
                         H5FD_FEAT_AGGREGATE_SMALLDATA | H5FD_FEAT_POSIX_COMPAT_HANDLE |
                         H5FD_FEAT_SUPPORTS_SWMR_IO | H5FD_FEAT_DEFAULT_VFD_COMPATIBLE))
        TEST_ERROR

    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;

    /* Retrieve the access property list... */
    if ((access_fapl = H5Fget_access_plist(file)) < 0)
        TEST_ERROR;

    /* Check that the driver is correct */
    if (H5FD_WINDOWS != H5Pget_driver(access_fapl))
        TEST_ERROR;

    /* ...and close the property list */
    if (H5Pclose(access_fapl) < 0)
        TEST_ERROR;

    /* Check file handle API */
    if (H5Fget_vfd_handle(file, H5P_DEFAULT, (void **)&fhandle) < 0)
        TEST_ERROR;
    if (*fhandle < 0)
        TEST_ERROR;

    /* Check file size API */
    if (H5Fget_filesize(file, &file_size) < 0)
        TEST_ERROR;

    /* There is no guarantee the size of metadata in file is constant.
     * Just try to check if it's reasonable.  It's 2KB right now.
     */
    if (file_size < 1 * KB || file_size > 4 * KB)
        TEST_ERROR;

    /* Close and delete the file */
    if (H5Fclose(file) < 0)
        TEST_ERROR;
    h5_delete_test_file(FILENAME[8], fapl);

    /* Close the fapl */
    if (H5Pclose(fapl) < 0)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(fapl);
        H5Fclose(file);
    }
    H5E_END_TRY;
    return -1;

#endif /* H5_HAVE_WINDOWS */
} /* end test_windows() */

/*-------------------------------------------------------------------------
 * Function:    test_ros3
 *
 * Purpose:     Tests the file handle interface for the ROS3 driver
 *
 *              As the ROS3 driver is 1) read only, 2) requires access
 *              to an S3 server (minio for now), this test is quite
 *              different from the other tests.
 *
 *              For now, test only fapl & flags.  Extend as the
 *              work on the VFD continues.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  John Mainzer
 *              7/12/17
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_ros3(void)
{
#ifdef H5_HAVE_ROS3_VFD
    hid_t            fid          = -1; /* file ID                      */
    hid_t            fapl_id      = -1; /* file access property list ID */
    hid_t            fapl_id_out  = -1; /* from H5Fget_access_plist     */
    hid_t            driver_id    = -1; /* ID for this VFD              */
    unsigned long    driver_flags = 0;  /* VFD feature flags            */
    char             filename[1024];    /* filename                     */
    H5FD_ros3_fapl_t test_ros3_fa;
    H5FD_ros3_fapl_t ros3_fa_0 = {
        /* version      = */ H5FD_CURR_ROS3_FAPL_T_VERSION,
        /* authenticate = */ FALSE,
        /* aws_region   = */ "",
        /* secret_id    = */ "",
        /* secret_key   = */ "plugh",
    };
#endif /*H5_HAVE_ROS3_VFD */

    TESTING("Read-only S3 file driver");

#ifndef H5_HAVE_ROS3_VFD
    SKIPPED();
    return 0;
#else  /* H5_HAVE_ROS3_VFD */

    /* Set property list and file name for ROS3 driver. */
    if ((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;

    if (H5Pset_fapl_ros3(fapl_id, &ros3_fa_0) < 0)
        TEST_ERROR;

    /* verify that the ROS3 FAPL entry is set as expected */
    if (H5Pget_fapl_ros3(fapl_id, &test_ros3_fa) < 0)
        TEST_ERROR;

    /* need a macro to compare instances of H5FD_ros3_fapl_t */
    if ((test_ros3_fa.version != ros3_fa_0.version) ||
        (test_ros3_fa.authenticate != ros3_fa_0.authenticate) ||
        (HDstrcmp(test_ros3_fa.aws_region, ros3_fa_0.aws_region) != 0) ||
        (HDstrcmp(test_ros3_fa.secret_id, ros3_fa_0.secret_id) != 0) ||
        (HDstrcmp(test_ros3_fa.secret_key, ros3_fa_0.secret_key) != 0))
        TEST_ERROR;

    h5_fixname(FILENAME[10], fapl_id, filename, sizeof(filename));

    /* Check that the VFD feature flags are correct */
    if ((driver_id = H5Pget_driver(fapl_id)) < 0)
        TEST_ERROR;

    if (H5FDdriver_query(driver_id, &driver_flags) < 0)
        TEST_ERROR;

    if (!(driver_flags & H5FD_FEAT_DATA_SIEVE))
        TEST_ERROR

    /* Check for extra flags not accounted for above */
    if (driver_flags != (H5FD_FEAT_DATA_SIEVE))
        TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(fapl_id);
        H5Pclose(fapl_id_out);
        H5Fclose(fid);
    }
    H5E_END_TRY;
    return -1;
#endif /* H5_HAVE_ROS3_VFD */
} /* end test_ros3() */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Macro: SPLITTER_TEST_FAULT()
 *
 * utility macro, helps create stack-like backtrace on error.
 * requires defined in the calling function:
 *    * variable `int ret_value` (return -1 on error)`
 *    * label `done` for exit on fault
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 */
#define SPLITTER_TEST_FAULT(mesg)                                                                            \
    {                                                                                                        \
        H5_FAILED();                                                                                         \
        AT();                                                                                                \
        HDfprintf(stderr, mesg);                                                                             \
        H5Eprint2(H5E_DEFAULT, stderr);                                                                      \
        HDfflush(stderr);                                                                                    \
        ret_value = -1;                                                                                      \
        goto done;                                                                                           \
    }

/*-------------------------------------------------------------------------
 * Function:    compare_splitter_config_info
 *
 * Purpose:     Helper function to compare configuration info found in a
 *              FAPL against a canonical structure.
 *
 * Return:      Success:  0, if config info in FAPL matches info structure.
 *              Failure: -1, if difference detected.
 *
 *-------------------------------------------------------------------------
 */
static int
compare_splitter_config_info(hid_t fapl_id, H5FD_splitter_vfd_config_t *info)
{
    int                         ret_value    = 0;
    H5FD_splitter_vfd_config_t *fetched_info = NULL;

    if (NULL == (fetched_info = HDcalloc(1, sizeof(H5FD_splitter_vfd_config_t))))
        SPLITTER_TEST_FAULT("memory allocation for fetched_info struct failed");

    fetched_info->magic      = H5FD_SPLITTER_MAGIC;
    fetched_info->version    = H5FD_CURR_SPLITTER_VFD_CONFIG_VERSION;
    fetched_info->rw_fapl_id = H5I_INVALID_HID;
    fetched_info->wo_fapl_id = H5I_INVALID_HID;

    if (H5Pget_fapl_splitter(fapl_id, fetched_info) < 0) {
        SPLITTER_TEST_FAULT("can't get splitter info");
    }
    if (info->rw_fapl_id == H5P_DEFAULT) {
        if (H5Pget_driver(fetched_info->rw_fapl_id) != H5Pget_driver(H5P_FILE_ACCESS_DEFAULT)) {
            SPLITTER_TEST_FAULT("Read-Write driver mismatch (default)\n");
        }
    }
    else {
        if (H5Pget_driver(fetched_info->rw_fapl_id) != H5Pget_driver(info->rw_fapl_id)) {
            SPLITTER_TEST_FAULT("Read-Write driver mismatch\n");
        }
    }
    if (info->wo_fapl_id == H5P_DEFAULT) {
        if (H5Pget_driver(fetched_info->wo_fapl_id) != H5Pget_driver(H5P_FILE_ACCESS_DEFAULT)) {
            SPLITTER_TEST_FAULT("Write-Only driver mismatch (default)\n");
        }
    }
    else {
        if (H5Pget_driver(fetched_info->wo_fapl_id) != H5Pget_driver(info->wo_fapl_id)) {
            SPLITTER_TEST_FAULT("Write-Only driver mismatch\n");
        }
    }
    if ((HDstrlen(info->wo_path) != HDstrlen(fetched_info->wo_path)) ||
        HDstrncmp(info->wo_path, fetched_info->wo_path, H5FD_SPLITTER_PATH_MAX) != 0) {
        HDfprintf(stderr, "MISMATCH: '%s' :: '%s'\n", info->wo_path, fetched_info->wo_path);
        HEXPRINT(H5FD_SPLITTER_PATH_MAX, info->wo_path);
        HEXPRINT(H5FD_SPLITTER_PATH_MAX, fetched_info->wo_path);
        SPLITTER_TEST_FAULT("Write-Only file path mismatch\n");
    }

done:
    HDfree(fetched_info);

    return ret_value;
} /* end compare_splitter_config_info() */

/*-------------------------------------------------------------------------
 * Function:    run_splitter_test
 *
 * Purpose:     Auxiliary function for test_splitter().
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Description:
 *              Perform basic open-write-close with the Splitter VFD.
 *              Prior to operations, removes files from a previous run,
 *              if they exist.
 *              After writing, compares read-write and write-only files.
 *              Includes FAPL sanity testing.
 *
 *-------------------------------------------------------------------------
 */
static int
run_splitter_test(const struct splitter_dataset_def *data, hbool_t ignore_wo_errors,
                  hbool_t provide_logfile_path, const hid_t sub_fapl_ids[2])
{
    hid_t                       file_id     = H5I_INVALID_HID;
    hid_t                       fapl_id     = H5I_INVALID_HID;
    hid_t                       dset_id     = H5I_INVALID_HID;
    hid_t                       space_id    = H5I_INVALID_HID;
    hid_t                       fapl_id_out = H5I_INVALID_HID;
    hid_t                       fapl_id_cpy = H5I_INVALID_HID;
    H5FD_splitter_vfd_config_t *vfd_config  = NULL;
    char *                      filename_rw = NULL;
    FILE *                      logfile     = NULL;
    int                         ret_value   = 0;

    if (NULL == (vfd_config = HDcalloc(1, sizeof(H5FD_splitter_vfd_config_t))))
        SPLITTER_TEST_FAULT("memory allocation for vfd_config struct failed");
    if (NULL == (filename_rw = HDcalloc(H5FD_SPLITTER_PATH_MAX + 1, sizeof(char))))
        SPLITTER_TEST_FAULT("memory allocation for filename_rw string failed");

    vfd_config->magic          = H5FD_SPLITTER_MAGIC;
    vfd_config->version        = H5FD_CURR_SPLITTER_VFD_CONFIG_VERSION;
    vfd_config->ignore_wo_errs = ignore_wo_errors;
    vfd_config->rw_fapl_id     = sub_fapl_ids[0];
    vfd_config->wo_fapl_id     = sub_fapl_ids[1];

    if (splitter_prepare_file_paths(vfd_config, filename_rw) < 0) {
        SPLITTER_TEST_FAULT("can't prepare file paths\n");
    }

    if (provide_logfile_path == FALSE) {
        vfd_config->log_file_path[0] = '\0'; /* reset as empty string */
    }

    /* Create a new fapl to use the SPLITTER file driver */
    if ((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) == H5I_INVALID_HID) {
        SPLITTER_TEST_FAULT("can't create FAPL ID\n");
    }
    if (H5Pset_fapl_splitter(fapl_id, vfd_config) < 0) {
        SPLITTER_TEST_FAULT("can't set splitter FAPL\n");
    }
    if (H5Pget_driver(fapl_id) != H5FD_SPLITTER) {
        SPLITTER_TEST_FAULT("set FAPL not SPLITTER\n");
    }

    if (compare_splitter_config_info(fapl_id, vfd_config) < 0) {
        SPLITTER_TEST_FAULT("information mismatch\n");
    }

    /*
     * Copy property list, light compare, and close the copy.
     * Helps test driver-implemented FAPL-copying and library ID management.
     */

    fapl_id_cpy = H5Pcopy(fapl_id);
    if (H5I_INVALID_HID == fapl_id_cpy) {
        SPLITTER_TEST_FAULT("can't copy FAPL\n");
    }
    if (compare_splitter_config_info(fapl_id_cpy, vfd_config) < 0) {
        SPLITTER_TEST_FAULT("information mismatch\n");
    }
    if (H5Pclose(fapl_id_cpy) < 0) {
        SPLITTER_TEST_FAULT("can't close fapl copy\n");
    }

    /*
     * Proceed with test. Create file.
     */
    file_id = H5Fcreate(filename_rw, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
    if (file_id < 0) {
        SPLITTER_TEST_FAULT("can't create file\n");
    }

    /*
     * Check driver from file
     */

    fapl_id_out = H5Fget_access_plist(file_id);
    if (H5I_INVALID_HID == fapl_id_out) {
        SPLITTER_TEST_FAULT("can't get file's FAPL\n");
    }
    if (H5Pget_driver(fapl_id_out) != H5FD_SPLITTER) {
        SPLITTER_TEST_FAULT("wrong file FAPL driver\n");
    }
    if (compare_splitter_config_info(fapl_id_out, vfd_config) < 0) {
        SPLITTER_TEST_FAULT("information mismatch\n");
    }
    if (H5Pclose(fapl_id_out) < 0) {
        SPLITTER_TEST_FAULT("can't close file's FAPL\n");
    }

    /*
     * Create and write the dataset
     */

    space_id = H5Screate_simple(data->n_dims, data->dims, NULL);
    if (space_id < 0) {
        SPLITTER_TEST_FAULT("can't create dataspace\n");
    }
    dset_id = H5Dcreate2(file_id, data->dset_name, data->mem_type_id, space_id, H5P_DEFAULT, H5P_DEFAULT,
                         H5P_DEFAULT);
    if (dset_id < 0) {
        SPLITTER_TEST_FAULT("can't create dataset\n");
    }
    if (H5Dwrite(dset_id, data->mem_type_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, data->buf) < 0) {
        SPLITTER_TEST_FAULT("can't write data to dataset\n");
    }

    /* Close everything */
    if (H5Dclose(dset_id) < 0) {
        SPLITTER_TEST_FAULT("can't close dset\n");
    }
    if (H5Sclose(space_id) < 0) {
        SPLITTER_TEST_FAULT("can't close space\n");
    }
    if (H5Pclose(fapl_id) < 0) {
        SPLITTER_TEST_FAULT("can't close fapl\n");
    }
    if (H5Fclose(file_id) < 0) {
        SPLITTER_TEST_FAULT("can't close file\n");
    }

    /* Verify that the R/W and W/O files are identical */
    if (h5_compare_file_bytes(filename_rw, vfd_config->wo_path) < 0) {
        SPLITTER_TEST_FAULT("files are not byte-for-byte equivalent\n");
    }

    /* Verify existence of logfile if appropriate */
    logfile = HDfopen(vfd_config->log_file_path, "r");
    if ((TRUE == provide_logfile_path && NULL == logfile) ||
        (FALSE == provide_logfile_path && NULL != logfile)) {
        SPLITTER_TEST_FAULT("no logfile when one was expected\n");
    }

done:
    if (ret_value < 0) {
        H5E_BEGIN_TRY
        {
            H5Dclose(dset_id);
            H5Sclose(space_id);
            H5Pclose(fapl_id_out);
            H5Pclose(fapl_id_cpy);
            H5Pclose(fapl_id);
            H5Fclose(file_id);
        }
        H5E_END_TRY;
    }

    if (logfile != NULL)
        HDfclose(logfile);

    HDfree(vfd_config);
    HDfree(filename_rw);

    return ret_value;
} /* end run_splitter_test() */

/*-------------------------------------------------------------------------
 * Function:    driver_is_splitter_compatible
 *
 * Purpose:     Determine whether the driver set in the FAPL ID is compatible
 *              with the Splitter VFD -- specificially, Write-Only channel.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Description: Attempts to put the given FAPL ID as the W/O channel.
 *              Uses driver's own mechanisms to generate error, and catches
 *              error.
 *
 *-------------------------------------------------------------------------
 */
static int
driver_is_splitter_compatible(hid_t fapl_id)
{
    H5FD_splitter_vfd_config_t *vfd_config    = NULL;
    hid_t                       split_fapl_id = H5I_INVALID_HID;
    herr_t                      ret           = SUCCEED;
    int                         ret_value     = 0;

    if (NULL == (vfd_config = HDcalloc(1, sizeof(H5FD_splitter_vfd_config_t)))) {
        FAIL_PUTS_ERROR("memory allocation for vfd_config struct failed");
    }

    if (H5I_INVALID_HID == (split_fapl_id = H5Pcreate(H5P_FILE_ACCESS))) {
        FAIL_PUTS_ERROR("Can't create contained FAPL");
    }
    vfd_config->magic          = H5FD_SPLITTER_MAGIC;
    vfd_config->version        = H5FD_CURR_SPLITTER_VFD_CONFIG_VERSION;
    vfd_config->ignore_wo_errs = FALSE;
    vfd_config->rw_fapl_id     = H5P_DEFAULT;
    vfd_config->wo_fapl_id     = fapl_id;
    HDstrncpy(vfd_config->wo_path, "nonesuch", H5FD_SPLITTER_PATH_MAX);
    vfd_config->log_file_path[0] = '\0';

    H5E_BEGIN_TRY
    {
        ret = H5Pset_fapl_splitter(split_fapl_id, vfd_config);
    }
    H5E_END_TRY;
    if (SUCCEED == ret) {
        ret_value = -1;
    }

    if (H5Pclose(split_fapl_id) < 0) {
        FAIL_PUTS_ERROR("Can't close contained FAPL")
    }
    split_fapl_id = H5I_INVALID_HID;

    HDfree(vfd_config);

    return ret_value;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(split_fapl_id);
    }
    H5E_END_TRY;

    HDfree(vfd_config);

    return -1;
} /* end driver_is_splitter_compatible() */

/*-------------------------------------------------------------------------
 * Function:    splitter_RO_test
 *
 * Purpose:     Verify Splitter VFD with the Read-Only access flag.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Description: Attempt read-only opening of files with different
 *              permutations of files already existing on-disk.
 *
 *-------------------------------------------------------------------------
 */
static int
splitter_RO_test(const struct splitter_dataset_def *data, hid_t child_fapl_id)
{
    char *                      filename_rw = NULL;
    H5FD_splitter_vfd_config_t *vfd_config  = NULL;
    hid_t                       fapl_id     = H5I_INVALID_HID;
    hid_t                       file_id     = H5I_INVALID_HID;
    int                         ret_value   = 0;

    if (NULL == (vfd_config = HDcalloc(1, sizeof(H5FD_splitter_vfd_config_t))))
        SPLITTER_TEST_FAULT("memory allocation for vfd_config struct failed");
    if (NULL == (filename_rw = HDcalloc(H5FD_SPLITTER_PATH_MAX + 1, sizeof(char))))
        SPLITTER_TEST_FAULT("memory allocation for filename_rw string failed");

    vfd_config->magic          = H5FD_SPLITTER_MAGIC;
    vfd_config->version        = H5FD_CURR_SPLITTER_VFD_CONFIG_VERSION;
    vfd_config->ignore_wo_errs = FALSE;
    vfd_config->rw_fapl_id     = child_fapl_id;
    vfd_config->wo_fapl_id     = child_fapl_id;

    if (splitter_prepare_file_paths(vfd_config, filename_rw) < 0) {
        SPLITTER_TEST_FAULT("can't prepare splitter file paths\n");
    }

    /* Create a new fapl to use the SPLITTER file driver */
    fapl_id = H5Pcreate(H5P_FILE_ACCESS);
    if (H5I_INVALID_HID == fapl_id) {
        SPLITTER_TEST_FAULT("can't create FAPL ID\n");
    }
    if (H5Pset_fapl_splitter(fapl_id, vfd_config) < 0) {
        SPLITTER_TEST_FAULT("can't set splitter FAPL\n");
    }
    if (H5Pget_driver(fapl_id) != H5FD_SPLITTER) {
        SPLITTER_TEST_FAULT("set FAPL not SPLITTER\n");
    }

    /* Attempt R/O open when both files are nonexistent
     * Should fail.
     */

    H5E_BEGIN_TRY
    {
        file_id = H5Fopen(filename_rw, H5F_ACC_RDONLY, fapl_id);
    }
    H5E_END_TRY;
    if (file_id >= 0) {
        SPLITTER_TEST_FAULT("R/O open on nonexistent files unexpectedly successful\n");
    }

    /* Attempt R/O open when only W/O file exists
     * Should fail.
     */

    if (splitter_create_single_file_at(vfd_config->wo_path, vfd_config->wo_fapl_id, data) < 0) {
        SPLITTER_TEST_FAULT("can't write W/O file\n");
    }
    H5E_BEGIN_TRY
    {
        file_id = H5Fopen(filename_rw, H5F_ACC_RDONLY, fapl_id);
    }
    H5E_END_TRY;
    if (file_id >= 0) {
        SPLITTER_TEST_FAULT("R/O open with extant W/O file unexpectedly successful\n");
    }
    HDremove(vfd_config->wo_path);

    /* Attempt R/O open when only R/W file exists
     * Should fail.
     */

    if (splitter_create_single_file_at(filename_rw, vfd_config->rw_fapl_id, data) < 0) {
        SPLITTER_TEST_FAULT("can't create R/W file\n");
    }
    H5E_BEGIN_TRY
    {
        file_id = H5Fopen(filename_rw, H5F_ACC_RDONLY, fapl_id);
    }
    H5E_END_TRY;
    if (file_id >= 0) {
        SPLITTER_TEST_FAULT("R/O open with extant R/W file unexpectedly successful\n");
    }

    /* Attempt R/O open when both R/W and W/O files exist
     */

    if (splitter_create_single_file_at(vfd_config->wo_path, vfd_config->wo_fapl_id, data) < 0) {
        SPLITTER_TEST_FAULT("can't create W/O file\n");
    }
    file_id = H5Fopen(filename_rw, H5F_ACC_RDONLY, fapl_id);
    if (file_id < 0) {
        SPLITTER_TEST_FAULT("R/O open on two extant files failed\n");
    }
    if (splitter_compare_expected_data(file_id, data) < 0) {
        SPLITTER_TEST_FAULT("data mismatch in R/W file\n");
    }
    if (H5Fclose(file_id) < 0) {
        SPLITTER_TEST_FAULT("can't close file(s)\n");
    }
    file_id = H5I_INVALID_HID;

    /* Cleanup
     */

    if (H5Pclose(fapl_id) < 0) {
        SPLITTER_TEST_FAULT("can't close FAPL ID\n");
    }
    fapl_id = H5I_INVALID_HID;

done:
    if (ret_value < 0) {
        H5E_BEGIN_TRY
        {
            H5Pclose(fapl_id);
            H5Fclose(file_id);
        }
        H5E_END_TRY;
    }

    HDfree(vfd_config);
    HDfree(filename_rw);

    return ret_value;
} /* end splitter_RO_test() */

/*-------------------------------------------------------------------------
 * Function:    splitter_prepare_file_paths
 *
 * Purpose:     Get file paths ready for use by the Splitter VFD tests.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Description:
 *              Use h5_fixname to adjust the splitter-relevant file paths
 *              from those given in FILENAMES.
 *
 *              REMOVES EXISTING FILES AT THE PATH LOCATIONS PRIOR TO RETURN.
 *
 *              The relevant file paths will be set in filename_rw_out and
 *              inside the config structure (wo_path, log_file_path).
 *
 *              `filename_rw_out` must be at least H5FD_SPLITTER_PATH_MAX+1
 *              characters long.
 *
 *              `vfd_config` must have its child FAPL IDs preset.
 *
 *-------------------------------------------------------------------------
 */
static int
splitter_prepare_file_paths(H5FD_splitter_vfd_config_t *vfd_config, char *filename_rw_out)
{
    int ret_value = 0;

    if (vfd_config == NULL || vfd_config->magic != H5FD_SPLITTER_MAGIC) {
        SPLITTER_TEST_FAULT("invalid splitter config pointer\n");
    }
    if (filename_rw_out == NULL) {
        SPLITTER_TEST_FAULT("NULL filename_rw pointer\n");
    }

    /* TODO: sanity-check fapl IDs? */

    /* Build the r/w file, w/o file, and the log file paths.
     * Output is stored in the associated string pointers.
     */
    h5_fixname(FILENAME[11], vfd_config->rw_fapl_id, filename_rw_out, H5FD_SPLITTER_PATH_MAX);
    h5_fixname(FILENAME[12], vfd_config->wo_fapl_id, vfd_config->wo_path, H5FD_SPLITTER_PATH_MAX);
    h5_fixname_no_suffix(FILENAME[13], vfd_config->wo_fapl_id, vfd_config->log_file_path,
                         H5FD_SPLITTER_PATH_MAX);

    /* Delete any existing files on disk.
     */
    HDremove(filename_rw_out);
    HDremove(vfd_config->wo_path);
    HDremove(vfd_config->log_file_path);

done:
    return ret_value;
} /* end splitter_prepare_file_paths() */

/*-------------------------------------------------------------------------
 * Function:    splitter_crate_single_file_at
 *
 * Purpose:     Create a file, optionally w/ dataset.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Description:
 *              Create a file at the given location with the given FAPL,
 *              and write data as defined in `data` in a pre-determined location in the file.
 *
 *              If the dataset definition pointer is NULL, no data is written
 *              to the file.
 *
 *              Will always overwrite an existing file with the given name/path.
 *
 *-------------------------------------------------------------------------
 */
static int
splitter_create_single_file_at(const char *filename, hid_t fapl_id, const struct splitter_dataset_def *data)
{
    hid_t file_id   = H5I_INVALID_HID;
    hid_t space_id  = H5I_INVALID_HID;
    hid_t dset_id   = H5I_INVALID_HID;
    int   ret_value = 0;

    if (filename == NULL || *filename == '\0') {
        SPLITTER_TEST_FAULT("filename is invalid\n");
    }
    /* TODO: sanity-check fapl id? */

    file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
    if (file_id < 0) {
        SPLITTER_TEST_FAULT("can't create file\n");
    }

    if (data) {
        /* TODO: sanity-check data, if it exists? */
        space_id = H5Screate_simple(data->n_dims, data->dims, NULL);
        if (space_id < 0) {
            SPLITTER_TEST_FAULT("can't create dataspace\n");
        }

        dset_id = H5Dcreate2(file_id, data->dset_name, data->mem_type_id, space_id, H5P_DEFAULT, H5P_DEFAULT,
                             H5P_DEFAULT);
        if (dset_id < 0) {
            SPLITTER_TEST_FAULT("can't create dataset\n");
        }

        if (H5Dwrite(dset_id, data->mem_type_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, data->buf) < 0) {
            SPLITTER_TEST_FAULT("can't write data to dataset\n");
        }

        if (H5Dclose(dset_id) < 0) {
            SPLITTER_TEST_FAULT("can't close dset\n");
        }
        if (H5Sclose(space_id) < 0) {
            SPLITTER_TEST_FAULT("can't close space\n");
        }
    } /* end if data definition is provided */

    if (H5Fclose(file_id) < 0) {
        SPLITTER_TEST_FAULT("can't close file\n");
    }

done:
    if (ret_value < 0) {
        H5E_BEGIN_TRY
        {
            H5Dclose(dset_id);
            H5Sclose(space_id);
            H5Fclose(file_id);
        }
        H5E_END_TRY;
    } /* end if error */
    return ret_value;
} /* end splitter_create_single_file_at() */

/*-------------------------------------------------------------------------
 * Function:    splitter_compare_expected_data
 *
 * Purpose:     Compare data within a predermined dataset.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Description: Read data from the file at a predetermined location, and
 *              compare its contents byte-for-byte with that expected in
 *              the `data` definition structure.
 *
 *-------------------------------------------------------------------------
 */
static int
splitter_compare_expected_data(hid_t file_id, const struct splitter_dataset_def *data)
{
    hid_t  dset_id = H5I_INVALID_HID;
    int    buf[SPLITTER_SIZE][SPLITTER_SIZE];
    int    expected[SPLITTER_SIZE][SPLITTER_SIZE];
    size_t i         = 0;
    size_t j         = 0;
    int    ret_value = 0;

    if (sizeof((void *)buf) != sizeof(data->buf)) {
        SPLITTER_TEST_FAULT("invariant size of expected data does not match that received!\n");
    }
    HDmemcpy(expected, data->buf, sizeof(expected));

    dset_id = H5Dopen2(file_id, data->dset_name, H5P_DEFAULT);
    if (dset_id < 0) {
        SPLITTER_TEST_FAULT("can't open dataset\n");
    }

    if (H5Dread(dset_id, data->mem_type_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, (void *)buf) < 0) {
        SPLITTER_TEST_FAULT("can't read dataset\n");
    }

    for (i = 0; i < SPLITTER_SIZE; i++) {
        for (j = 0; j < SPLITTER_SIZE; j++) {
            if (buf[i][j] != expected[i][j]) {
                SPLITTER_TEST_FAULT("mismatch in expected data\n");
            }
        }
    }

    if (H5Dclose(dset_id) < 0) {
        SPLITTER_TEST_FAULT("can't close dataset\n");
    }

done:
    if (ret_value < 0) {
        H5E_BEGIN_TRY
        {
            H5Dclose(dset_id);
        }
        H5E_END_TRY;
    }
    return ret_value;
} /* end splitter_compare_expected_data() */

/*-------------------------------------------------------------------------
 * Function:    splitter_tentative_open_test()
 *
 * Purpose:     Verifies Splitter behavior with "tentative" H5F_open.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Description:
 *              H5F_open() has a two-stage opening process when given a
 *              Read/Write access flag -- first it performs a "tentative
 *              open", where it checks to see whether files already exist
 *              on the system, done in such a way as to not "alter its state"
 *              (i.e., truncate).
 *              This can cause problems with the Splitter VFD, as the
 *              file on the R/W channel might exist already, but that on the
 *              W/O channel will not, and vice-versa.
 *
 *              This test exists to verify that in any event, files will be
 *              created as required.
 *
 *-------------------------------------------------------------------------
 */
static int
splitter_tentative_open_test(hid_t child_fapl_id)
{
    const char *                filename_tmp = "splitter_tmp.h5";
    char *                      filename_rw  = NULL;
    H5FD_splitter_vfd_config_t *vfd_config   = NULL;
    hid_t                       fapl_id      = H5I_INVALID_HID;
    hid_t                       file_id      = H5I_INVALID_HID;
    int                         buf[SPLITTER_SIZE][SPLITTER_SIZE];        /* for comparison */
    hsize_t                     dims[2] = {SPLITTER_SIZE, SPLITTER_SIZE}; /* for comparison */
    int                         i       = 0;                              /* for comparison */
    int                         j       = 0;                              /* for comparison */
    struct splitter_dataset_def data;                                     /* for comparison */
    int                         ret_value = 0;

    if (NULL == (vfd_config = HDcalloc(1, sizeof(H5FD_splitter_vfd_config_t))))
        SPLITTER_TEST_FAULT("memory allocation for vfd_config struct failed");
    if (NULL == (filename_rw = HDcalloc(H5FD_SPLITTER_PATH_MAX + 1, sizeof(char))))
        SPLITTER_TEST_FAULT("memory allocation for filename_rw string failed");

    /* pre-fill data buffer to write */
    for (i = 0; i < SPLITTER_SIZE; i++) {
        for (j = 0; j < SPLITTER_SIZE; j++) {
            buf[i][j] = i * 100 + j;
        }
    }

    /* Dataset info */
    data.buf         = (void *)buf;
    data.mem_type_id = H5T_NATIVE_INT;
    data.dims        = dims;
    data.n_dims      = 2;
    data.dset_name   = SPLITTER_DATASET_NAME;

    vfd_config->magic          = H5FD_SPLITTER_MAGIC;
    vfd_config->version        = H5FD_CURR_SPLITTER_VFD_CONFIG_VERSION;
    vfd_config->ignore_wo_errs = FALSE;
    vfd_config->rw_fapl_id     = child_fapl_id;
    vfd_config->wo_fapl_id     = child_fapl_id;

    if (splitter_prepare_file_paths(vfd_config, filename_rw) < 0) {
        SPLITTER_TEST_FAULT("can't prepare splitter file paths\n");
    }

    /* Create a new fapl to use the SPLITTER file driver */
    if ((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) == H5I_INVALID_HID) {
        SPLITTER_TEST_FAULT("can't create FAPL ID\n");
    }
    if (H5Pset_fapl_splitter(fapl_id, vfd_config) < 0) {
        SPLITTER_TEST_FAULT("can't set splitter FAPL\n");
    }
    if (H5Pget_driver(fapl_id) != H5FD_SPLITTER) {
        SPLITTER_TEST_FAULT("set FAPL not SPLITTER\n");
    }

    /* Create instance of file on disk.
     * Will be copied verbatim as needed, to avoid issues where differences in
     * the creation time would befoul comparisons.
     */
    if (splitter_create_single_file_at(filename_tmp, child_fapl_id, &data) < 0) {
        SPLITTER_TEST_FAULT("can't write W/O file\n");
    }

    /*
     * H5Fopen() with RDWR access.
     * Neither file exist already
     * Should fail.
     */

    H5E_BEGIN_TRY
    {
        file_id = H5Fopen(filename_rw, H5F_ACC_RDWR, fapl_id);
    }
    H5E_END_TRY;
    if (file_id != H5I_INVALID_HID) {
        SPLITTER_TEST_FAULT("open with both nonexistent files unexpectedly succeeded\n");
    }
    if (file_exists(filename_rw, child_fapl_id)) {
        SPLITTER_TEST_FAULT("R/W file unexpectedly created\n");
    }
    if (file_exists(vfd_config->wo_path, child_fapl_id)) {
        SPLITTER_TEST_FAULT("W/O file unexpectedly created\n");
    }

    /*
     * H5Fopen() with RDWR access.
     * Only W/O file present.
     * Should fail.
     */

    if (h5_duplicate_file_by_bytes(filename_tmp, vfd_config->wo_path) < 0) {
        SPLITTER_TEST_FAULT("Can't create W/O file copy.\n");
    }
    H5E_BEGIN_TRY
    {
        file_id = H5Fopen(filename_rw, H5F_ACC_RDWR, fapl_id);
    }
    H5E_END_TRY;
    if (file_id != H5I_INVALID_HID) {
        SPLITTER_TEST_FAULT("open with nonexistent R/W file unexpectedly succeeded\n");
    }
    if (file_exists(filename_rw, child_fapl_id)) {
        SPLITTER_TEST_FAULT("R/W file unexpectedly created\n");
    }
    if (!file_exists(vfd_config->wo_path, child_fapl_id)) {
        SPLITTER_TEST_FAULT("W/O file mysteriously disappeared\n");
    }
    HDremove(vfd_config->wo_path);
    if (file_exists(vfd_config->wo_path, child_fapl_id)) {
        SPLITTER_TEST_FAULT("failed to remove W/O file\n");
    }

    /*
     * H5Fopen() with RDWR access.
     * Only R/W file present.
     * Should fail.
     */

    if (h5_duplicate_file_by_bytes(filename_tmp, filename_rw) < 0) {
        SPLITTER_TEST_FAULT("Can't create R/W file copy.\n");
    }
    H5E_BEGIN_TRY
    {
        file_id = H5Fopen(filename_rw, H5F_ACC_RDWR, fapl_id);
    }
    H5E_END_TRY;
    if (file_id != H5I_INVALID_HID) {
        SPLITTER_TEST_FAULT("open with nonexistent W/O unexpectedly succeeded\n");
    }
    if (!file_exists(filename_rw, child_fapl_id)) {
        SPLITTER_TEST_FAULT("R/W file mysteriously disappeared\n");
    }
    if (file_exists(vfd_config->wo_path, child_fapl_id)) {
        SPLITTER_TEST_FAULT("W/O file unexpectedly created\n");
    }

    /*
     * H5Fopen() with RDWR access.
     * Both files present.
     */

    if (h5_duplicate_file_by_bytes(filename_tmp, vfd_config->wo_path) < 0) {
        SPLITTER_TEST_FAULT("Can't create W/O file copy.\n");
    }
    file_id = H5Fopen(filename_rw, H5F_ACC_RDWR, fapl_id);
    if (file_id == H5I_INVALID_HID) {
        SPLITTER_TEST_FAULT("file-open failed with both present\n");
    }
    /* Open successful; close file then inspect presence again */
    if (H5Fclose(file_id) < 0) {
        SPLITTER_TEST_FAULT("can't close file ID\n");
    }
    if (!file_exists(filename_rw, child_fapl_id)) {
        SPLITTER_TEST_FAULT("R/W file mysteriously disappared\n");
    }
    if (!file_exists(vfd_config->wo_path, child_fapl_id)) {
        SPLITTER_TEST_FAULT("W/O file mysteriously disappeared\n");
    }

    /*
     * H5Fcreate() with TRUNC access.
     * Both files present.
     */

    file_id = H5Fcreate(filename_rw, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
    if (file_id == H5I_INVALID_HID) {
        SPLITTER_TEST_FAULT("file-open failed with both present\n");
    }
    /* Open successful; close file then inspect presence again */
    if (H5Fclose(file_id) < 0) {
        SPLITTER_TEST_FAULT("can't close file ID\n");
    }
    if (!file_exists(filename_rw, child_fapl_id)) {
        SPLITTER_TEST_FAULT("R/W file mysteriously disappared\n");
    }
    if (!file_exists(vfd_config->wo_path, child_fapl_id)) {
        SPLITTER_TEST_FAULT("W/O file mysteriously disappeared\n");
    }
    if (h5_compare_file_bytes(filename_rw, vfd_config->wo_path) < 0) {
        SPLITTER_TEST_FAULT("files are not byte-for-byte equivalent\n");
    }
    HDremove(filename_rw);
    HDremove(vfd_config->wo_path);

    /*
     * H5Fcreate() with TRUNC access.
     * R/W already exists.
     */

    if (h5_duplicate_file_by_bytes(filename_tmp, filename_rw) < 0) {
        SPLITTER_TEST_FAULT("Can't create R/W file copy.\n");
    }
    if (file_exists(vfd_config->wo_path, child_fapl_id)) {
        SPLITTER_TEST_FAULT("failed to remove W/O file\n");
    }
    file_id = H5Fcreate(filename_rw, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
    if (file_id == H5I_INVALID_HID) {
        SPLITTER_TEST_FAULT("file-open failed with both present\n");
    }
    /* Open successful; close file then inspect presence again */
    if (H5Fclose(file_id) < 0) {
        SPLITTER_TEST_FAULT("can't close file ID\n");
    }
    if (!file_exists(filename_rw, child_fapl_id)) {
        SPLITTER_TEST_FAULT("R/W file mysteriously disappared\n");
    }
    if (!file_exists(vfd_config->wo_path, child_fapl_id)) {
        SPLITTER_TEST_FAULT("W/O file mysteriously disappeared\n");
    }
    if (h5_compare_file_bytes(filename_rw, vfd_config->wo_path) < 0) {
        SPLITTER_TEST_FAULT("files are not byte-for-byte equivalent\n");
    }
    HDremove(filename_rw);
    HDremove(vfd_config->wo_path);

    /*
     * H5Fcreate() with TRUNC access.
     * Only W/O present.
     */

    if (h5_duplicate_file_by_bytes(filename_tmp, vfd_config->wo_path) < 0) {
        SPLITTER_TEST_FAULT("Can't create W/O file copy.\n");
    }
    if (file_exists(filename_rw, child_fapl_id)) {
        SPLITTER_TEST_FAULT("failed to remove R/W file\n");
    }
    file_id = H5Fcreate(filename_rw, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
    if (file_id == H5I_INVALID_HID) {
        SPLITTER_TEST_FAULT("file-open failed with both present\n");
    }
    /* Open successful; close file then inspect presence again */
    if (H5Fclose(file_id) < 0) {
        SPLITTER_TEST_FAULT("can't close file ID\n");
    }
    if (!file_exists(filename_rw, child_fapl_id)) {
        SPLITTER_TEST_FAULT("R/W file mysteriously disappared\n");
    }
    if (!file_exists(vfd_config->wo_path, child_fapl_id)) {
        SPLITTER_TEST_FAULT("W/O file mysteriously disappeared\n");
    }
    if (h5_compare_file_bytes(filename_rw, vfd_config->wo_path) < 0) {
        SPLITTER_TEST_FAULT("files are not byte-for-byte equivalent\n");
    }
    HDremove(filename_rw);
    HDremove(vfd_config->wo_path);

    /* H5Fcreate with both files absent is tested elsewhere */

    /*
     * Cleanup
     */

    if (H5Pclose(fapl_id) < 0) {
        SPLITTER_TEST_FAULT("can't close splitter FAPL ID\n");
    }

done:
    if (ret_value < 0) {
        H5E_BEGIN_TRY
        {
            H5Pclose(fapl_id);
            H5Fclose(file_id);
        }
        H5E_END_TRY;
    }

    HDfree(vfd_config);
    HDfree(filename_rw);

    return ret_value;
} /* end splitter_tentative_open_test() */

/*-------------------------------------------------------------------------
 * Function:    file_exists()
 *
 * Purpose:     Determine whether a file exists on-system
 *
 * Return:      Non-zero (1) if it exists (H5Fopen successful),
 *              zero (0) if absent (cannot be opened).
 *
 * Description: Attempt H5Fopen with the given FAPL ID and RDONLY access flag.
 *
 *-------------------------------------------------------------------------
 */
static int
file_exists(const char *filename, hid_t fapl_id)
{
    hid_t file_id   = H5I_INVALID_HID;
    int   ret_value = 0;

    H5E_BEGIN_TRY
    {
        file_id = H5Fopen(filename, H5F_ACC_RDONLY, fapl_id);
    }
    H5E_END_TRY;
    if (file_id != H5I_INVALID_HID) {
        ret_value = 1;
        if (H5Fclose(file_id) < 0) {
            FAIL_PUTS_ERROR("can't close file ID\n");
        }
    }

    return ret_value;

error:
    H5E_BEGIN_TRY
    {
        H5Fclose(file_id);
    }
    H5E_END_TRY;
    return ret_value;
} /* end file_exists() */

/*-------------------------------------------------------------------------
 * Function:    test_splitter
 *
 * Purpose:     Tests the Splitter VFD
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Description:
 *              This test function uses the Splitter VFD to produce a r/w
 *              file and a w/o file. It will verify that the two files
 *              are identical.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_splitter(void)
{
    int                         buf[SPLITTER_SIZE][SPLITTER_SIZE];
    hsize_t                     dims[2]       = {SPLITTER_SIZE, SPLITTER_SIZE};
    hid_t                       child_fapl_id = H5I_INVALID_HID;
    int                         i             = 0;
    int                         j             = 0;
    struct splitter_dataset_def data;

    TESTING("SPLITTER file driver");

    /* pre-fill data buffer to write */
    for (i = 0; i < SPLITTER_SIZE; i++) {
        for (j = 0; j < SPLITTER_SIZE; j++) {
            buf[i][j] = i * 100 + j;
        }
    }

    /* Dataset info */
    data.buf         = (void *)buf;
    data.mem_type_id = H5T_NATIVE_INT;
    data.dims        = dims;
    data.n_dims      = 2;
    data.dset_name   = SPLITTER_DATASET_NAME;

    /* Stand-in for manual FAPL creation
     * Enables verification with arbitrary VFDs via `make check-vfd`
     */
    child_fapl_id = h5_fileaccess();
    if (child_fapl_id < 0) {
        TEST_ERROR;
    }

    if (!driver_is_splitter_compatible(child_fapl_id)) {
        SKIPPED();
        HDprintf("  given driver is not Splitter W/O compatible.\n");
        return 0;
    }

    /* Test Read-Only access, including when a file on the W/O channel
     * does not exist.
     */
    if (splitter_RO_test(&data, child_fapl_id) < 0) {
        TEST_ERROR;
    }

    /* Test opening of files when the W/O channel does not exist.
     */
    if (splitter_tentative_open_test(child_fapl_id) < 0) {
        TEST_ERROR;
    }

    /* Test file creation, utilizing different child FAPLs (default vs.
     * specified), logfile, and Write Channel error ignoring behavior.
     */
    for (i = 0; i < 4; i++) {
        hbool_t ignore_wo_errors     = (i & 1) ? TRUE : FALSE;
        hbool_t provide_logfile_path = (i & 2) ? TRUE : FALSE;
        hid_t   child_fapl_ids[2]    = {H5P_DEFAULT, H5P_DEFAULT};

        /* Test child driver definition/default combination */
        for (j = 0; j < 4; j++) {

            child_fapl_ids[0] = (j & 1) ? child_fapl_id : H5P_DEFAULT;
            child_fapl_ids[1] = (j & 2) ? child_fapl_id : H5P_DEFAULT;

            if (run_splitter_test(&data, ignore_wo_errors, provide_logfile_path, child_fapl_ids) < 0) {
                TEST_ERROR;
            }

        } /* end for child fapl definition/pairing */

    } /* end for behavior-flag loops */

    /* TODO: SWMR open? */
    /* Concurrent opens with both drivers using the Splitter */

    if (H5Pclose(child_fapl_id) == FAIL) {
        TEST_ERROR;
    }

    PASSED();
    return 0;

error:
    if (child_fapl_id != H5I_INVALID_HID)
        H5Pclose(child_fapl_id);

    return -1;
} /* end test_splitter() */

#undef SPLITTER_TEST_FAULT

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Tests the basic features of Virtual File Drivers
 *
 * Return:      EXIT_SUCCESS/EXIT_FAILURE
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    int nerrors = 0;

    h5_reset();

    HDprintf("Testing basic Virtual File Driver functionality.\n");

    nerrors += test_sec2() < 0 ? 1 : 0;
    nerrors += test_core() < 0 ? 1 : 0;
    nerrors += test_direct() < 0 ? 1 : 0;
    nerrors += test_family() < 0 ? 1 : 0;
    nerrors += test_family_compat() < 0 ? 1 : 0;
    nerrors += test_family_member_fapl() < 0 ? 1 : 0;
    nerrors += test_multi() < 0 ? 1 : 0;
    nerrors += test_multi_compat() < 0 ? 1 : 0;
    nerrors += test_log() < 0 ? 1 : 0;
    nerrors += test_stdio() < 0 ? 1 : 0;
    nerrors += test_windows() < 0 ? 1 : 0;
    nerrors += test_ros3() < 0 ? 1 : 0;
    nerrors += test_splitter() < 0 ? 1 : 0;

    if (nerrors) {
        HDprintf("***** %d Virtual File Driver TEST%s FAILED! *****\n", nerrors, nerrors > 1 ? "S" : "");
        return EXIT_FAILURE;
    }

    HDprintf("All Virtual File Driver tests passed.\n");

    return EXIT_SUCCESS;
} /* end main() */
