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

/***********************************************************
*
* Test program:	 
*
* Tests the VFD SWMR Feature.
*
*************************************************************/

#include "h5test.h"

/*
 * This file needs to access private information from the H5F package.
 */
#define H5MF_FRIEND		/*suppress error about including H5MFpkg	  */
#include "H5MFpkg.h"

#define H5F_FRIEND		/*suppress error about including H5Fpkg	  */
#define H5F_TESTING
#include "H5Fpkg.h"

#include "H5CXprivate.h"        /* API Contexts                         */
#include "H5Iprivate.h"
#include "H5PBprivate.h"


#define FILENAME_LEN		1024
#define NUM_DSETS               5
#define NX                      100
#define NY                      50

static unsigned open_file(char *filename, hid_t fapl, hsize_t page_size, size_t page_buffer_size);

/* test routines for VFD SWMR */
static unsigned test_fapl();
static unsigned test_file_end_tick();
static unsigned test_file_fapl();

const char *FILENAME[] = {
    "filepaged",
    NULL
};


/*********************/
/*********************/


/*-------------------------------------------------------------------------
 * Function:    test_fapl()
 *
 * Purpose:     A) Verify that invalid info set in the fapl fails
 *                 as expected (see the RFC for VFD SWMR):
 *                 --version: should be a known version
 *                 --tick_len: should be >= 0
 *                 --max_lag: should be >= 3
 *                 --md_pages_reserved: should be >= 1
 *                 --md_file_path: should contain the metadata file path (POSIX)
 *              B) Verify that info set in the fapl is retrieved correctly.
 *
 * Return:      0 if test is sucessful
 *              1 if test fails
 *
 * Programmer:  Vailin Choi; July 2018
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_fapl()
{
    hid_t fapl = -1;    /* File access property list */
    H5F_vfd_swmr_config_t *my_config = NULL;   /* Configuration for VFD SWMR */
    herr_t ret;         /* Return value */

    TESTING("Configure VFD SWMR with fapl");

    /* Allocate memory for the configuration structure */
    if((my_config = (H5F_vfd_swmr_config_t *)HDmalloc(sizeof(H5F_vfd_swmr_config_t))) == NULL)
        FAIL_STACK_ERROR;
    HDmemset(my_config, 0, sizeof(H5F_vfd_swmr_config_t));

    /* Get a copy of the file access property list */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;

    /* Should get invalid VFD SWMR config info */
    if(H5Pget_vfd_swmr_config(fapl, my_config) < 0)
        TEST_ERROR;

    /* Verify that the version is incorrect */
    if(my_config->version >= H5F__CURR_VFD_SWMR_CONFIG_VERSION)
        TEST_ERROR;

    /* Should fail: version is 0 */
    H5E_BEGIN_TRY {
        ret = H5Pset_vfd_swmr_config(fapl, my_config);
    } H5E_END_TRY;
    if(ret >= 0)
        TEST_ERROR;

    /* Set valid version */
    my_config->version = H5F__CURR_VFD_SWMR_CONFIG_VERSION;
    /* Should fail: tick_len is -1 */
    my_config->tick_len = -1;
    H5E_BEGIN_TRY {
        ret = H5Pset_vfd_swmr_config(fapl, my_config);
    } H5E_END_TRY;
    if(ret >= 0)
        TEST_ERROR;

    /* Set valid tick_len */
    my_config->tick_len = 3; 
    /* Should fail: max_lag is 2 */
    my_config->max_lag = 2;
    H5E_BEGIN_TRY {
        ret = H5Pset_vfd_swmr_config(fapl, my_config);
    } H5E_END_TRY;
    if(ret >= 0)
        TEST_ERROR;

    /* Set valid max_lag */
    my_config->max_lag = 3;
    /* Should fail: md_pages_reserved is 0 */
    H5E_BEGIN_TRY {
        ret = H5Pset_vfd_swmr_config(fapl, my_config);
    } H5E_END_TRY;
    if(ret >= 0)
        TEST_ERROR;

    /* Set valid md_pages_reserved */
    my_config->md_pages_reserved = 2;
    /* Should fail: empty md_file_path */
    H5E_BEGIN_TRY {
        ret = H5Pset_vfd_swmr_config(fapl, my_config);
    } H5E_END_TRY;
    if(ret >= 0)
        TEST_ERROR;

    /* Set md_file_path */
    HDstrcpy(my_config->md_file_path, "my_md_file");
    my_config->vfd_swmr_writer = TRUE;

    /* Should succeed in setting the configuration info */
    if(H5Pset_vfd_swmr_config(fapl, my_config) < 0)
        TEST_ERROR;

    /* Clear the configuration structure */
    HDmemset(my_config, 0, sizeof(H5F_vfd_swmr_config_t));

    /* Retrieve the configuration info just set */
    if(H5Pget_vfd_swmr_config(fapl, my_config) < 0)
        TEST_ERROR;

    /* Verify the configuration info */
    if(my_config->version < H5F__CURR_VFD_SWMR_CONFIG_VERSION)
        TEST_ERROR;
    if(my_config->md_pages_reserved != 2)
        TEST_ERROR;
    if(HDstrcmp(my_config->md_file_path, "my_md_file") != 0)
        TEST_ERROR;

    /* Close the file access property list */
    if(H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR;

    if(my_config)
        HDfree(my_config);

    PASSED()
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(fapl);
    } H5E_END_TRY;
    if(my_config)
        HDfree(my_config);
    return 1;
} /* test_fapl() */


/*-------------------------------------------------------------------------
 * Function:    test_file_fapl()
 *
 * Purpose:     A) Verify that page buffering and paged aggregation
 *                 have to be enabled for a file to be configured
 *                 with VFD SWMR.
 *              B) Verify the VFD SWMR configuration set in fapl
 *                 used to create/open the file is the same as the
 *                 configuration retrieved from the file's fapl.
 *
 * Return:      0 if test is sucessful
 *              1 if test fails
 *
 * Programmer:  Vailin Choi; July 2018
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_file_fapl()
{
    hid_t fid = -1;         /* File ID */
    hid_t fcpl = -1;        /* File creation property list ID */
    hid_t fapl1 = -1;       /* File access property list ID */
    hid_t fapl2 = -1;       /* File access property list ID */
    hid_t file_fapl = -1;   /* File access property list ID associated with the file */
    H5F_vfd_swmr_config_t *config1 = NULL;   /* Configuration for VFD SWMR */
    H5F_vfd_swmr_config_t *config2 = NULL;   /* Configuration for VFD SWMR */
    H5F_vfd_swmr_config_t *config3 = NULL;   /* Configuration for VFD SWMR */
    herr_t ret;             /* Return value */
hid_t fid_read = -1;    /* File ID for VFD SWMR reader */
hid_t sid = -1;
hid_t did = -1;

    TESTING("VFD SWMR configuration for the file and fapl");

    /* Should succeed without VFD SWMR configured */
    if((fid = H5Fcreate("myfile", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Close the file  */
    if(H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Allocate memory for the configuration structure */
    if((config1 = (H5F_vfd_swmr_config_t *)HDmalloc(sizeof(H5F_vfd_swmr_config_t))) == NULL)
        FAIL_STACK_ERROR;
    if((config2 = (H5F_vfd_swmr_config_t *)HDmalloc(sizeof(H5F_vfd_swmr_config_t))) == NULL)
        FAIL_STACK_ERROR;
    if((config3 = (H5F_vfd_swmr_config_t *)HDmalloc(sizeof(H5F_vfd_swmr_config_t))) == NULL)
        FAIL_STACK_ERROR;
    HDmemset(config1, 0, sizeof(H5F_vfd_swmr_config_t));
    HDmemset(config2, 0, sizeof(H5F_vfd_swmr_config_t));
    HDmemset(config3, 0, sizeof(H5F_vfd_swmr_config_t));

    /* Create a copy of the file access property list */
    if((fapl1 = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;

    config1->version = H5F__CURR_VFD_SWMR_CONFIG_VERSION;
    config1->tick_len = 4; 
    config1->max_lag = 6;
    config1->vfd_swmr_writer = TRUE;
    config1->md_pages_reserved = 2;
    HDstrcpy(config1->md_file_path, "my_md_file");

    /* Should succeed in setting the VFD SWMR configuration */
    if(H5Pset_vfd_swmr_config(fapl1, config1) < 0)
        TEST_ERROR;

    /* Should fail to configure VFD SWMR: page buffering and paged aggregation not enabled */
    H5E_BEGIN_TRY {
        fid = H5Fcreate("myfile", H5F_ACC_TRUNC, H5P_DEFAULT, fapl1);
    } H5E_END_TRY;
    if(fid >= 0)
        TEST_ERROR;

    /* Create a copy of file creation property list */
    if((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
            FAIL_STACK_ERROR

    /* Set file space strategy */
    if(H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_PAGE, FALSE, (hsize_t)1) < 0)
        FAIL_STACK_ERROR;

    /* Should fail to configure VFD SWMR: no page buffering */
    H5E_BEGIN_TRY {
        fid = H5Fcreate("myfile", H5F_ACC_TRUNC, fcpl, fapl1);
    } H5E_END_TRY;
    if(fid >= 0)
        TEST_ERROR;

    /* Enable page buffering */
    if(H5Pset_page_buffer_size(fapl1, 4096, 0, 0) < 0)
        FAIL_STACK_ERROR;

    /* Should succeed to configure VFD SWMR: paged aggregation and page buffering enabled */
    if((fid = H5Fcreate("myfile", H5F_ACC_TRUNC, fcpl, fapl1)) < 0)
        TEST_ERROR;

    /* Get the file's file access property list */
    if((file_fapl = H5Fget_access_plist(fid)) < 0)
        FAIL_STACK_ERROR;

    /* Retrieve the VFD SWMR configuration from file_fapl */
    if(H5Pget_vfd_swmr_config(file_fapl, config2) < 0)
        TEST_ERROR;

    /* Verify the retrieved info is the same as config1 */
    if(HDmemcmp(config1, config2, sizeof(H5F_vfd_swmr_config_t)) != 0)
        TEST_ERROR;

    /* Closing */
    if(H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;
    if(H5Pclose(file_fapl) < 0)
        FAIL_STACK_ERROR;


    /* Should succeed: VFD SWMR writer */
    if((fid = H5Fopen("myfile", H5F_ACC_RDWR, fapl1)) < 0)
        TEST_ERROR;

    /* Get the file's file access property list */
    if((file_fapl = H5Fget_access_plist(fid)) < 0)
        FAIL_STACK_ERROR;

    /* Clear info in config2 */
    HDmemset(config2, 0, sizeof(H5F_vfd_swmr_config_t));

    /* Retrieve the VFD SWMR configuration from file_fapl */
    if(H5Pget_vfd_swmr_config(file_fapl, config2) < 0)
        TEST_ERROR;

    /* Verify the retrieved info is the same as config1 */
    if(HDmemcmp(config1, config2, sizeof(H5F_vfd_swmr_config_t)) != 0)
        TEST_ERROR;

    /* Closing */
    if(H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;
    if(H5Pclose(file_fapl) < 0)
        FAIL_STACK_ERROR;

    /* Set a different VFD SWMR configuration */
    /* Create a copy of the file access property list */
    if((fapl2 = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;

    config3->version = H5F__CURR_VFD_SWMR_CONFIG_VERSION;
    config3->tick_len = 4; 
    config3->max_lag = 10;
    config3->vfd_swmr_writer = TRUE;
    config3->md_pages_reserved = 2;
    HDstrcpy(config3->md_file_path, "my_md_file");

    /* Should succeed in setting the VFD SWMR configuration */
    if(H5Pset_vfd_swmr_config(fapl2, config3) < 0)
        TEST_ERROR;

    /* Enable page buffering */
    if(H5Pset_page_buffer_size(fapl2, 4096, 0, 0) < 0)
        FAIL_STACK_ERROR;

    /* Should succeed: VFD SWMR writer */
    if((fid = H5Fopen("myfile", H5F_ACC_RDWR, fapl2)) < 0)
        TEST_ERROR;

    /* Get the file's file access property list */
    if((file_fapl = H5Fget_access_plist(fid)) < 0)
        FAIL_STACK_ERROR;

    /* Clear info in config2 */
    HDmemset(config2, 0, sizeof(H5F_vfd_swmr_config_t));

    /* Retrieve the VFD SWMR configuration from file_fapl */
    if(H5Pget_vfd_swmr_config(file_fapl, config2) < 0)
        TEST_ERROR;

    /* Verify the retrieved info is NOT the same as config1 */
    if(HDmemcmp(config1, config2, sizeof(H5F_vfd_swmr_config_t)) == 0)
        TEST_ERROR;

    /* Verify the retrieved info is the same as config3 */
    if(HDmemcmp(config2, config3, sizeof(H5F_vfd_swmr_config_t)) != 0)
        TEST_ERROR;

    /* Closing */
    if(H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;
    if(H5Pclose(file_fapl) < 0)
        FAIL_STACK_ERROR;

    /* 
     * VDF SWMR READER
     */
    /* Create a copy of the file access property list */
    if((fapl2 = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;

    config3->version = H5F__CURR_VFD_SWMR_CONFIG_VERSION;
    config3->tick_len = 4; 
    config3->max_lag = 10;
    config3->vfd_swmr_writer = FALSE;
    config3->md_pages_reserved = 2;
    HDstrcpy(config3->md_file_path, "my_md_file");

    /* Should succeed in setting the VFD SWMR configuration */
    if(H5Pset_vfd_swmr_config(fapl2, config3) < 0)
        TEST_ERROR;

    /* Enable page buffering */
    if(H5Pset_page_buffer_size(fapl2, 4096, 0, 0) < 0)
        FAIL_STACK_ERROR;


    /* Open the file for reading only */
    H5E_BEGIN_TRY {
        fid_read = H5Fopen("myfile", H5F_ACC_RDONLY, fapl2);
    } H5E_END_TRY;
    if(fid_read >= 0)
        TEST_ERROR;

    if(H5Pclose(fapl1) < 0)
        FAIL_STACK_ERROR;
    if(H5Pclose(fapl2) < 0)
        FAIL_STACK_ERROR;
    if(H5Pclose(fcpl) < 0)
        FAIL_STACK_ERROR;
    if(config1)
        HDfree(config1);
    if(config2)
        HDfree(config2);
    if(config3)
        HDfree(config3);

    PASSED()
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(fapl1);
        H5Pclose(fapl2);
        H5Pclose(fcpl);
        H5Fclose(fid);
    } H5E_END_TRY;
    if(config1)
        HDfree(config1);
    if(config2)
        HDfree(config2);
    if(config3)
        HDfree(config3);
    return 1;
} /* test_file_fapl() */


/*-------------------------------------------------------------------------
 * Function:    test_file_end_tick()
 *
 * Purpose:     Verify the public routine H5Fvfd_swmr_end_tick() works
 *              as described in the RFC for VFD SWMR.
 *              --routine will fail if the file is not opened with VFD SWMR
 *              ?? Will add more tests when end of tick processing 
 *                 is activated in this routine
 *
 * Return:      0 if test is sucessful
 *              1 if test fails
 *
 * Programmer:  Vailin Choi; July 2018
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_file_end_tick()
{
    hid_t fid = -1;     /* File ID */
    hid_t fapl = -1;    /* File access property list */
    hid_t fcpl = -1;    /* File creation property list */
    H5F_vfd_swmr_config_t *my_config = NULL;    /* Configuration for VFD SWMR */
    herr_t ret;         /* Return value */

    TESTING("H5Fvfd_swmr_end_tick() for VFD SWMR");

    /* Should succeed without VFD SWMR configured */
    if((fid = H5Fcreate("myfile", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Should fail */
    H5E_BEGIN_TRY {
        ret = H5Fvfd_swmr_end_tick(fid);
    } H5E_END_TRY;
    if(ret >= 0)
        TEST_ERROR;

    /* Close the file  */
    if(H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Allocate memory for the configuration structure */
    if((my_config = (H5F_vfd_swmr_config_t *)HDmalloc(sizeof(H5F_vfd_swmr_config_t))) == NULL)
        FAIL_STACK_ERROR;
    HDmemset(my_config, 0, sizeof(H5F_vfd_swmr_config_t));

    /* Create a copy of the file access property list */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        FAIL_STACK_ERROR;
    
    /* Set the configuration */
    my_config->version = H5F__CURR_VFD_SWMR_CONFIG_VERSION;
    my_config->tick_len = 3; 
    my_config->max_lag = 3;
    my_config->vfd_swmr_writer = TRUE;
    my_config->md_pages_reserved = 2;
    HDstrcpy(my_config->md_file_path, "my_md_file");

    /* Should succeed in setting the VFD SWMR configuration */
    if(H5Pset_vfd_swmr_config(fapl, my_config) < 0)
        TEST_ERROR;

    /* Enable page buffering */
    if(H5Pset_page_buffer_size(fapl, 4096, 0, 0) < 0)
        FAIL_STACK_ERROR;

    /* Create a copy of file creation property list */
    if((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
        FAIL_STACK_ERROR

    /* Set file space strategy */
    if(H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_PAGE, FALSE, (hsize_t)1) < 0)
        FAIL_STACK_ERROR;

    /* Create the file with VFD SWMR configured */
    if((fid = H5Fcreate("myfile", H5F_ACC_TRUNC, fcpl, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Should succeed */
    if(H5Fvfd_swmr_end_tick(fid) < 0)
        TEST_ERROR;

    /* Close the file */
    if(H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Open the file as VFD SWMR writer */
    if((fid = H5Fopen("myfile", H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR;

    /* Should succeed */
    if(H5Fvfd_swmr_end_tick(fid) < 0)
        TEST_ERROR;

    /* Close the file */
    if(H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

#ifdef NOTYET
    /* Open the file as VFD SWMR reader */
    if((fid = H5Fopen("myfile", H5F_ACC_RDONLY, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Should succeed */
    if(H5Fvfd_swmr_end_tick(fid) < 0)
        TEST_ERROR;

    /* Close the file */
    if(H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;
#endif

    /* Open the file as writer without VFD SWMR configured */
    if((fid = H5Fopen("myfile", H5F_ACC_RDWR, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Should fail */
    H5E_BEGIN_TRY {
        ret = H5Fvfd_swmr_end_tick(fid);
    } H5E_END_TRY;
    if(ret >= 0)
        TEST_ERROR;

    /* Close the file */
    if(H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Open the file as reader without VFD SWMR configured */
    if((fid = H5Fopen("myfile", H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Should fail */
    H5E_BEGIN_TRY {
        ret = H5Fvfd_swmr_end_tick(fid);
    } H5E_END_TRY;
    if(ret >= 0)
        TEST_ERROR;

    /* Close the file */
    if(H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    if(H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR;
    if(H5Pclose(fcpl) < 0)
        FAIL_STACK_ERROR;
    if(my_config)
        HDfree(my_config);

    PASSED()
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(fapl);
        H5Pclose(fcpl);
        H5Fclose(fid);
    } H5E_END_TRY;

    if(my_config)
        HDfree(my_config);

    return 1;
} /* test_file_end_tick() */


/*-------------------------------------------------------------------------
 * Function:    main()
 *
 * Purpose:     Main function for VFD SWMR tests.
 *
 * Return:      0 if test is sucessful
 *              1 if test fails
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t       fapl = -1;              /* File access property list for data files */
    unsigned    nerrors = 0;            /* Cumulative error count */
    const char  *env_h5_drvr = NULL;    /* File Driver value from environment */
    hbool_t     api_ctx_pushed = FALSE;             /* Whether API context pushed */

    h5_reset();

    /* Get the VFD to use */
    env_h5_drvr = HDgetenv("HDF5_DRIVER");
    if(env_h5_drvr == NULL)
        env_h5_drvr = "nomatch";

    /* Temporary skip testing with multi/split drivers:
     * Page buffering depends on paged aggregation which is
     * currently disabled for multi/split drivers.
     */
    if((0 == HDstrcmp(env_h5_drvr, "multi")) || 
       (0 == HDstrcmp(env_h5_drvr, "split"))) {

        SKIPPED()
        HDputs("Skip VFD SWMR test because paged aggregation is disabled for multi/split drivers");
        HDexit(EXIT_SUCCESS);
    } /* end if */

    if((fapl = h5_fileaccess()) < 0) {
        nerrors++;
        PUTS_ERROR("Can't get VFD-dependent fapl")
    } /* end if */

    /* Push API context */
    if(H5CX_push() < 0) FAIL_STACK_ERROR
        api_ctx_pushed = TRUE;

    nerrors += test_fapl();
    nerrors += test_file_fapl();
    nerrors += test_file_end_tick();

    h5_clean_files(FILENAME, fapl);

    if(nerrors)
        goto error;

    /* Pop API context */
    if(api_ctx_pushed && H5CX_pop() < 0) FAIL_STACK_ERROR
        api_ctx_pushed = FALSE;

    HDputs("All VFD SWMR tests passed.");

    HDexit(EXIT_SUCCESS);

error:
    HDprintf("***** %d VFD SWMR TEST%s FAILED! *****\n",
        nerrors, nerrors > 1 ? "S" : "");

    H5E_BEGIN_TRY {
        H5Pclose(fapl);
    } H5E_END_TRY;

    if(api_ctx_pushed) H5CX_pop();

    HDexit(EXIT_FAILURE);
} /* main() */

