/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by Akadio, Inc.                                                 *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
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

#include "H5queue.h"
#include "h5test.h"
#include "vfd_swmr_common.h"

/*
 * This file needs to access private information from the H5F package.
 */

#define H5F_FRIEND  /*suppress error about including H5Fpkg */
#define H5FD_FRIEND /*suppress error about including H5FDpkg */
#define H5F_TESTING
#define H5FD_TESTING
#include "H5FDprivate.h"
#include "H5Fpkg.h"
#include "H5FDpkg.h"
#include "H5Iprivate.h"

#define H5FD_FRIEND /*suppress error about including H5FDpkg      */
#include "H5FDpkg.h"

#define FS_PAGE_SIZE 512
#define FILENAME     "vfd_swmr_file.h5"
#define MD_FILENAME  "vfd_swmr_metadata_file"
#define UD_FILENAME  "vfd_swmr_updater_file"

#define FILENAME2    "vfd_swmr_file2.h5"
#define MD_FILENAME2 "vfd_swmr_metadata_file2"

#define FILENAME3    "vfd_swmr_file3.h5"
#define MD_FILENAME3 "vfd_swmr_metadata_file3"

#define FILENAME4 "vfd_swmr_file4.h5"
#define MD_FILE   "vfd_swmr_md_file"
#define UD_FILE   "vfd_swmr_ud_file"

#define FNAME "non_vfd_swmr_file.h5"

#define FILE_NAME_LEN 1024

/* Defines used by verify_updater_flags() and verify_ud_chk() helper routine */

/* Offset of "flags" in updater file header */
#define UD_HD_FLAGS_OFFSET 6

/* Offset of "sequence number" in updater file header */
#define UD_HD_SEQ_NUM_OFFSET 12

/* Offset of "change list length" in updater file header */
#define UD_HD_CHANGE_LIST_LEN_OFFSET 36

/* Offset of "number of change list entries" in updater file change list header */
#define UD_CL_NUM_CHANGE_LIST_ENTRIES_OFFSET H5F_UD_HEADER_SIZE + 44

/* Size of "sequence number", "tick number" and "change list length" fields in the updater file header */
#define UD_SIZE_8 8

/* Size of checksum and "number of change list entries" field in the updater change list header */
#define UD_SIZE_4 4

/* Size of "flags" field in the updater file header */
#define UD_SIZE_2 2

/* test routines for VFD SWMR */
static unsigned test_fapl(void);
static unsigned test_file_end_tick(void);
static unsigned test_file_fapl(void);
static unsigned test_writer_md(void);

static unsigned test_updater_flags(void);
static unsigned test_updater_flags_same_file_opens(void);
static herr_t   verify_updater_flags(char *ud_name, uint16_t expected_flags);

static void     clean_chk_ud_files(char *md_file_path, char *updater_file_path);
static herr_t   verify_ud_chk(char *md_file_path, char *ud_file_path);
static herr_t   md_ck_cb(char *md_file_path, uint64_t tick_num);
static unsigned test_updater_generate_md_checksums(hbool_t file_create);

/*-------------------------------------------------------------------------
 * Function:    test_fapl()
 *
 * Purpose:     A) Verify that invalid info set in the fapl fails
 *                 as expected (see the RFC for VFD SWMR):
 *                 --version: should be a known version
 *                 --tick_len: should be >= 0
 *                 --max_lag: should be >= 3
 *                 --md_pages_reserved: should be >= 2
 *                 --md_file_path: should contain the metadata file path (POSIX)
 *                 --at least one of maintain_metadata_file and generate_updater_files
 *                   must be true
 *                 --if both the writer and maintain_metadata_file fields are true,
 *                   then md_file_path field shouldn't be empty
 *                 --if both the writer and generate_updater_files fields are true,
 *                   then updater_file_path field shouldn't be empty
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
test_fapl(void)
{
    hid_t                  fapl      = -1;   /* File access property list */
    H5F_vfd_swmr_config_t *my_config = NULL; /* Configuration for VFD SWMR */
    herr_t                 ret;              /* Return value */

    TESTING("Configure VFD SWMR with fapl");

    /* Allocate memory for the configuration structure */
    if ((my_config = HDmalloc(sizeof(*my_config))) == NULL)
        FAIL_STACK_ERROR;
    HDmemset(my_config, 0, sizeof(*my_config));

    /* Get a copy of the file access property list */
    if ((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;

    /* Should get invalid VFD SWMR config info */
    if (H5Pget_vfd_swmr_config(fapl, my_config) < 0)
        TEST_ERROR;

    /* Verify that the version is incorrect */
    if (my_config->version >= H5F__CURR_VFD_SWMR_CONFIG_VERSION)
        TEST_ERROR;

    /* Should fail: version is 0 */
    H5E_BEGIN_TRY
    {
        ret = H5Pset_vfd_swmr_config(fapl, my_config);
    }
    H5E_END_TRY;
    if (ret >= 0)
        TEST_ERROR;

    /* Set valid version */
    my_config->version = H5F__CURR_VFD_SWMR_CONFIG_VERSION;

    /* Set valid tick_len */
    my_config->tick_len = 3;
    /* Should fail: max_lag is 2 */
    my_config->max_lag = 2;
    H5E_BEGIN_TRY
    {
        ret = H5Pset_vfd_swmr_config(fapl, my_config);
    }
    H5E_END_TRY;
    if (ret >= 0)
        TEST_ERROR;

    /* Set valid max_lag */
    my_config->max_lag = 3;
    /* Should fail: md_pages_reserved is 0 */
    H5E_BEGIN_TRY
    {
        ret = H5Pset_vfd_swmr_config(fapl, my_config);
    }
    H5E_END_TRY;
    if (ret >= 0)
        TEST_ERROR;

    /* Set valid md_pages_reserved */
    my_config->md_pages_reserved = 2;
    /* Should fail: empty md_file_path */
    H5E_BEGIN_TRY
    {
        ret = H5Pset_vfd_swmr_config(fapl, my_config);
    }
    H5E_END_TRY;
    if (ret >= 0)
        TEST_ERROR;

    my_config->writer = TRUE;
    /* Should fail: at least one of maintain_metadata_file and generate_updater_files must be true */
    H5E_BEGIN_TRY
    {
        ret = H5Pset_vfd_swmr_config(fapl, my_config);
    }
    H5E_END_TRY;
    if (ret >= 0)
        TEST_ERROR;

    my_config->writer                 = TRUE;
    my_config->maintain_metadata_file = TRUE;
    /* Should fail: empty md_file_path */
    H5E_BEGIN_TRY
    {
        ret = H5Pset_vfd_swmr_config(fapl, my_config);
    }
    H5E_END_TRY;
    if (ret >= 0)
        TEST_ERROR;

    my_config->generate_updater_files = TRUE;
    /* Should fail: empty updater_file_path */
    H5E_BEGIN_TRY
    {
        ret = H5Pset_vfd_swmr_config(fapl, my_config);
    }
    H5E_END_TRY;
    if (ret >= 0)
        TEST_ERROR;

    /* Set md_file_path */
    HDstrcpy(my_config->md_file_path, MD_FILENAME);
    my_config->generate_updater_files = FALSE;

    /* Should succeed in setting the configuration info */
    if (H5Pset_vfd_swmr_config(fapl, my_config) < 0)
        TEST_ERROR;

    /* Clear the configuration structure */
    HDmemset(my_config, 0, sizeof(H5F_vfd_swmr_config_t));

    /* Retrieve the configuration info just set */
    if (H5Pget_vfd_swmr_config(fapl, my_config) < 0)
        TEST_ERROR;

    /* Verify the configuration info */
    if (my_config->version < H5F__CURR_VFD_SWMR_CONFIG_VERSION)
        TEST_ERROR;
    if (my_config->md_pages_reserved != 2)
        TEST_ERROR;
    if (my_config->generate_updater_files)
        TEST_ERROR;
    if (HDstrcmp(my_config->md_file_path, MD_FILENAME) != 0)
        TEST_ERROR;

    my_config->generate_updater_files = TRUE;
    /* Set updater_file_path */
    HDstrcpy(my_config->updater_file_path, UD_FILENAME);

    /* Should succeed in setting the configuration info */
    if (H5Pset_vfd_swmr_config(fapl, my_config) < 0)
        TEST_ERROR;

    /* Clear the configuration structure */
    HDmemset(my_config, 0, sizeof(H5F_vfd_swmr_config_t));

    /* Retrieve the configuration info just set */
    if (H5Pget_vfd_swmr_config(fapl, my_config) < 0)
        TEST_ERROR;

    /* Verify the configuration info */
    if (!my_config->generate_updater_files)
        TEST_ERROR;
    if (HDstrcmp(my_config->updater_file_path, UD_FILENAME) != 0)
        TEST_ERROR;
    if (!my_config->maintain_metadata_file)
        TEST_ERROR;
    if (HDstrcmp(my_config->md_file_path, MD_FILENAME) != 0)
        TEST_ERROR;

    /* Close the file access property list */
    if (H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR;

    if (my_config)
        HDfree(my_config);

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(fapl);
    }
    H5E_END_TRY;
    if (my_config)
        HDfree(my_config);
    return 1;
} /* test_fapl() */

/*-------------------------------------------------------------------------
 * Function:    test_file_fapl()
 *
 * Purpose:     A) Verify that page buffering and paged aggregation
 *                 have to be enabled for a file to be configured
 *                 with VFD SWMR.
 *              B) Verify that the "writer" setting in the fapl's VFD
 *                 SWMR configuration should be consistent with the
 *                 file access flags.
 *              C) Verify the VFD SWMR configuration set in fapl
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
test_file_fapl(void)
{
    hid_t                  fid         = -1;   /* File ID */
    hid_t                  fid2        = -1;   /* File ID */
    hid_t                  fcpl        = -1;   /* File creation property list ID */
    hid_t                  fapl1       = -1;   /* File access property list ID */
    hid_t                  fapl2       = -1;   /* File access property list ID */
    hid_t                  file_fapl   = -1;   /* File access property list ID associated with the file */
    H5F_vfd_swmr_config_t *config1     = NULL; /* Configuration for VFD SWMR */
    H5F_vfd_swmr_config_t *config2     = NULL; /* Configuration for VFD SWMR */
    H5F_vfd_swmr_config_t *file_config = NULL; /* Configuration for VFD SWMR */

    TESTING("VFD SWMR configuration for the file and fapl");

    /* Should succeed without VFD SWMR configured */
    if ((fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Close the file  */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Allocate memory for the configuration structure */
    if ((config1 = (H5F_vfd_swmr_config_t *)HDmalloc(sizeof(H5F_vfd_swmr_config_t))) == NULL)
        FAIL_STACK_ERROR;
    if ((config2 = (H5F_vfd_swmr_config_t *)HDmalloc(sizeof(H5F_vfd_swmr_config_t))) == NULL)
        FAIL_STACK_ERROR;
    if ((file_config = (H5F_vfd_swmr_config_t *)HDmalloc(sizeof(H5F_vfd_swmr_config_t))) == NULL)
        FAIL_STACK_ERROR;

    /*
     * Configured as VFD SWMR reader + no page buffering
     */

    /* config, tick_len, max_lag, writer, maintain_metadata_file, generate_updater_files,
       flush_raw_data, md_pages_reserved, md_file_path, updater_file_path */
    init_vfd_swmr_config(config1, 4, 7, FALSE, TRUE, FALSE, TRUE, 2, MD_FILENAME, NULL);

    /* use_latest_format, use_vfd_swmr, only_meta_page, page_buf_size, config */
    fapl1 = vfd_swmr_create_fapl(FALSE, TRUE, FALSE, 0, config1);
    if (fapl1 == H5I_INVALID_HID)
        TEST_ERROR;

    /* Should fail to create: file access is writer but VFD SWMR config is reader */
    H5E_BEGIN_TRY
    {
        fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl1);
    }
    H5E_END_TRY;
    if (fid >= 0)
        TEST_ERROR;

    if (H5Pclose(fapl1) < 0)
        FAIL_STACK_ERROR;

    /*
     * Configured as VFD SWMR writer + no page buffering
     */

    /* config, tick_len, max_lag, writer, maintain_metadata_file, generate_updater_files,
       flush_raw_data, md_pages_reserved, md_file_path, updater_file_path */
    init_vfd_swmr_config(config1, 4, 7, TRUE, TRUE, TRUE, TRUE, 2, MD_FILENAME, UD_FILENAME);

    /* use_latest_format, use_vfd_swmr, only_meta_page, page_buf_size, config */
    fapl1 = vfd_swmr_create_fapl(FALSE, TRUE, FALSE, 0, config1);

    if (fapl1 == H5I_INVALID_HID)
        TEST_ERROR;

    /* Should fail to create: page buffering and paged aggregation not enabled */
    H5E_BEGIN_TRY
    {
        fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl1);
    }
    H5E_END_TRY;
    if (fid >= 0)
        TEST_ERROR;

    if ((fcpl = vfd_swmr_create_fcpl(H5F_FSPACE_STRATEGY_PAGE, 4096)) < 0) {
        HDprintf("vfd_swmr_create_fcpl() failed");
        FAIL_STACK_ERROR;
    }

    /* Should fail to create: no page buffering */
    H5E_BEGIN_TRY
    {
        fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, fapl1);
    }
    H5E_END_TRY;
    if (fid >= 0)
        TEST_ERROR;

    if (H5Pclose(fapl1) < 0)
        FAIL_STACK_ERROR;

    /*
     * Configured as VFD SWMR writer + page buffering
     */

    /* config, tick_len, max_lag, writer, maintain_metadata_file, generate_updater_files,
       flush_raw_data, md_pages_reserved, md_file_path, updater_file_path */
    init_vfd_swmr_config(config1, 4, 7, TRUE, TRUE, FALSE, TRUE, 2, MD_FILENAME, NULL);

    /* use_latest_format, use_vfd_swmr, only_meta_page, page_buf_size, config */
    fapl1 = vfd_swmr_create_fapl(FALSE, TRUE, FALSE, 4096, config1);

    if (fapl1 == H5I_INVALID_HID)
        TEST_ERROR;

    /* Should succeed to create the file: paged aggregation and page buffering enabled */
    if ((fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, fapl1)) < 0)
        TEST_ERROR;

    /* Get the file's file access property list */
    if ((file_fapl = H5Fget_access_plist(fid)) < 0)
        FAIL_STACK_ERROR;

    /* Retrieve the VFD SWMR configuration from file_fapl */
    if (H5Pget_vfd_swmr_config(file_fapl, file_config) < 0)
        TEST_ERROR;

    /* Verify the retrieved info is the same as config1 */
    if (HDmemcmp(config1, file_config, sizeof(H5F_vfd_swmr_config_t)) != 0)
        TEST_ERROR;

    /* Closing */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(file_fapl) < 0)
        FAIL_STACK_ERROR;

    /* Should fail to open: file access is reader but VFD SWMR config is writer */
    H5E_BEGIN_TRY
    {
        fid = H5Fopen(FILENAME, H5F_ACC_RDONLY, fapl1);
    }
    H5E_END_TRY;
    if (fid >= 0)
        TEST_ERROR;

    /* Should succeed to open: file access and VFD SWMR config are consistent */
    if ((fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl1)) < 0)
        TEST_ERROR;

    /* Get the file's file access property list */
    if ((file_fapl = H5Fget_access_plist(fid)) < 0)
        FAIL_STACK_ERROR;

    /* Clear info in file_config */
    HDmemset(file_config, 0, sizeof(H5F_vfd_swmr_config_t));

    /* Retrieve the VFD SWMR configuration from file_fapl */
    if (H5Pget_vfd_swmr_config(file_fapl, file_config) < 0)
        TEST_ERROR;

    /* Verify the retrieved info is the same as config1 */
    if (HDmemcmp(config1, file_config, sizeof(H5F_vfd_swmr_config_t)) != 0)
        TEST_ERROR;

    /* Closing */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(file_fapl) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(fapl1) < 0)
        FAIL_STACK_ERROR;

    /*
     * Set up different VFD SWMR configuration + page_buffering
     */

    /* config, tick_len, max_lag, writer, maintain_metadata_file, generate_updater_files,
       flush_raw_data, md_pages_reserved, md_file_path, updater_file_path */
    init_vfd_swmr_config(config2, 4, 10, TRUE, TRUE, FALSE, TRUE, 2, MD_FILENAME, NULL);

    /* use_latest_format, use_vfd_swmr, only_meta_page, page_buf_size, config */
    fapl2 = vfd_swmr_create_fapl(FALSE, TRUE, FALSE, 4096, config2);

    if (fapl2 == H5I_INVALID_HID)
        TEST_ERROR;

    /* Should succeed to open the file as VFD SWMR writer */
    if ((fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl2)) < 0)
        TEST_ERROR;

    /* Get the file's file access property list */
    if ((file_fapl = H5Fget_access_plist(fid)) < 0)
        FAIL_STACK_ERROR;

    /* Clear info in file_config */
    HDmemset(file_config, 0, sizeof(H5F_vfd_swmr_config_t));

    /* Retrieve the VFD SWMR configuration from file_fapl */
    if (H5Pget_vfd_swmr_config(file_fapl, file_config) < 0)
        TEST_ERROR;

    /* Verify the retrieved info is NOT the same as config1 */
    if (HDmemcmp(config1, file_config, sizeof(H5F_vfd_swmr_config_t)) == 0)
        TEST_ERROR;

    /* Verify the retrieved info is the same as config2 */
    if (HDmemcmp(config2, file_config, sizeof(H5F_vfd_swmr_config_t)) != 0)
        TEST_ERROR;

    /* The file previously opened as VDF SWMR writer is still open */
    /* with VFD SWMR configuration in config2 */

    /*
     * Set up as VFD SWMR writer in config1 but different from config2
     */

    /* config, tick_len, max_lag, writer, maintain_metadata_file, generate_updater_files,
       flush_raw_data, md_pages_reserved, md_file_path, updater_file_path */
    init_vfd_swmr_config(config1, 3, 8, TRUE, TRUE, FALSE, TRUE, 3, MD_FILENAME, NULL);

    /* use_latest_format, use_vfd_swmr, only_meta_page, page_buf_size, config */
    fapl1 = vfd_swmr_create_fapl(FALSE, TRUE, FALSE, 4096, config1);

    if (fapl1 == H5I_INVALID_HID)
        TEST_ERROR;

    /* Re-open the same file with config1 */
    /* Should fail to open since config1 is different from config2 setting */
    H5E_BEGIN_TRY
    {
        fid2 = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl1);
    }
    H5E_END_TRY;
    if (fid2 >= 0)
        TEST_ERROR;

    /* Close fapl1 */
    if (H5Pclose(fapl1) < 0)
        FAIL_STACK_ERROR;

    /*
     * Set up as VFD SWMR reader in config1 which is same as config2
     */

    init_vfd_swmr_config(config1, 4, 10, TRUE, TRUE, FALSE, TRUE, 2, MD_FILENAME, NULL);

    /* use_latest_format, use_vfd_swmr, only_meta_page, page_buf_size, config */
    fapl1 = vfd_swmr_create_fapl(FALSE, TRUE, FALSE, 4096, config1);

    if (fapl1 == H5I_INVALID_HID)
        TEST_ERROR;

    /* Re-open the same file as VFD SWMR writer */
    /* Should succeed since config1 is same as the setting in config2 */
    if ((fid2 = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl1)) < 0)
        TEST_ERROR;

    /* Close fapl1 */
    if (H5Pclose(fapl1) < 0)
        FAIL_STACK_ERROR;

    HDmemset(file_config, 0, sizeof(H5F_vfd_swmr_config_t));

    /* Get the file's file access property list */
    if ((file_fapl = H5Fget_access_plist(fid)) < 0)
        FAIL_STACK_ERROR;

    /* Retrieve the VFD SWMR configuration from file_fapl */
    if (H5Pget_vfd_swmr_config(file_fapl, file_config) < 0)
        TEST_ERROR;

    /* Should be the same as config1 */
    if (HDmemcmp(config1, file_config, sizeof(H5F_vfd_swmr_config_t)) != 0)
        TEST_ERROR;

    /* Should be the the same as config2 */
    if (HDmemcmp(config2, file_config, sizeof(H5F_vfd_swmr_config_t)) != 0)
        TEST_ERROR;

    /* Closing */
    if (H5Fclose(fid2) < 0)
        FAIL_STACK_ERROR;
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    if (H5Pclose(fapl2) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(file_fapl) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(fcpl) < 0)
        FAIL_STACK_ERROR;

    /* Free buffers */
    if (config1)
        HDfree(config1);
    if (config2)
        HDfree(config2);
    if (file_config)
        HDfree(file_config);

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(fapl1);
        H5Pclose(fapl2);
        H5Pclose(file_fapl);
        H5Pclose(fcpl);
        H5Fclose(fid);
        H5Fclose(fid2);
    }
    H5E_END_TRY;
    if (config1)
        HDfree(config1);
    if (config2)
        HDfree(config2);
    if (file_config)
        HDfree(file_config);
    return 1;
} /* test_file_fapl() */

/*-------------------------------------------------------------------------
 * Function:    test_file_end_tick()
 *
 * Purpose:     Verify the public routine H5Fvfd_swmr_end_tick() works
 *              as described in the RFC for VFD SWMR.
 *
 * Return:      0 if test is sucessful
 *              1 if test fails
 *
 * Programmer:  Vailin Choi; June 2020
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_file_end_tick(void)
{
    hid_t                  fid1    = H5I_INVALID_HID; /* File ID */
    hid_t                  fid2    = H5I_INVALID_HID; /* File ID */
    hid_t                  fid3    = H5I_INVALID_HID; /* File ID */
    hid_t                  fcpl    = H5I_INVALID_HID; /* File creation property list ID */
    hid_t                  fapl1   = H5I_INVALID_HID; /* File access property list ID */
    hid_t                  fapl2   = H5I_INVALID_HID; /* File access property list ID */
    hid_t                  fapl3   = H5I_INVALID_HID; /* File access property list ID */
    H5F_vfd_swmr_config_t *config1 = NULL;            /* Configuration for VFD SWMR */
    H5F_vfd_swmr_config_t *config2 = NULL;            /* Configuration for VFD SWMR */
    H5F_vfd_swmr_config_t *config3 = NULL;            /* Configuration for VFD SWMR */
    H5F_t *                f1, *f2, *f3;              /* File pointer */
    uint64_t               s1 = 0;                    /* Saved tick_num */
    uint64_t               s2 = 0;                    /* Saved tick_num */
    uint64_t               s3 = 0;                    /* Saved tick_num */
    int                    ret;                       /* Return status */

    TESTING("H5Fvfd_swmr_end_tick()");

    /* Create a file without VFD SWMR configured */
    if ((fid1 = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Should fail to trigger EOT */
    H5E_BEGIN_TRY
    {
        ret = H5Fvfd_swmr_end_tick(fid1);
    }
    H5E_END_TRY;
    if (ret >= 0)
        TEST_ERROR;

    /* Close the file  */
    if (H5Fclose(fid1) < 0)
        FAIL_STACK_ERROR;

    /* Allocate memory for the configuration structure */
    if ((config1 = HDmalloc(sizeof(*config1))) == NULL)
        FAIL_STACK_ERROR;
    if ((config2 = HDmalloc(sizeof(*config2))) == NULL)
        FAIL_STACK_ERROR;
    if ((config3 = HDmalloc(sizeof(*config3))) == NULL)
        FAIL_STACK_ERROR;

    /*
     * Configured file 1 as VFD SWMR writer + page buffering
     */

    /* config, tick_len, max_lag, writer, maintain_metadata_file, generate_updater_files,
       flush_raw_data, md_pages_reserved, md_file_path, updater_file_path */
    init_vfd_swmr_config(config1, 10, 15, TRUE, TRUE, FALSE, TRUE, 2, MD_FILENAME, NULL);

    /* use_latest_format, use_vfd_swmr, only_meta_page, page_buf_size, config */
    fapl1 = vfd_swmr_create_fapl(FALSE, TRUE, FALSE, 4096, config1);

    if (fapl1 == H5I_INVALID_HID)
        TEST_ERROR;

    /*
     * Configured file 2 as VFD SWMR writer + page buffering
     */

    /* config, tick_len, max_lag, writer, maintain_metadata_file, generate_updater_files,
       flush_raw_data, md_pages_reserved, md_file_path, updater_file_path */
    init_vfd_swmr_config(config2, 5, 6, TRUE, TRUE, FALSE, TRUE, 2, MD_FILENAME2, NULL);

    /* use_latest_format, use_vfd_swmr, only_meta_page, page_buf_size, config */
    fapl2 = vfd_swmr_create_fapl(FALSE, TRUE, FALSE, 4096, config2);

    if (fapl2 == H5I_INVALID_HID)
        TEST_ERROR;

    /*
     * Configured file 3 as VFD SWMR writer + page buffering
     */

    /* config, tick_len, max_lag, writer, maintain_metadata_file, generate_updater_files,
       flush_raw_data, md_pages_reserved, md_file_path, updater_file_path */
    init_vfd_swmr_config(config3, 3, 6, TRUE, TRUE, FALSE, TRUE, 2, MD_FILENAME3, NULL);

    /* use_latest_format, use_vfd_swmr, only_meta_page, page_buf_size, config */
    fapl3 = vfd_swmr_create_fapl(FALSE, TRUE, FALSE, 4096, config3);

    if (fapl3 == H5I_INVALID_HID)
        TEST_ERROR;

    if ((fcpl = vfd_swmr_create_fcpl(H5F_FSPACE_STRATEGY_PAGE, 4096)) < 0) {
        HDprintf("vfd_swmr_create_fcpl() failed");
        FAIL_STACK_ERROR;
    }

    /* Create file 1 with VFD SWMR writer */
    if ((fid1 = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, fapl1)) < 0)
        TEST_ERROR;
    /* Keep file 1 opened */

    /* Create file 2 with VFD SWMR writer */
    if ((fid2 = H5Fcreate(FILENAME2, H5F_ACC_TRUNC, fcpl, fapl2)) < 0)
        TEST_ERROR;
    if (H5Fclose(fid2) < 0)
        FAIL_STACK_ERROR;

    /* Create file 3 with VFD SWMR writer */
    if ((fid3 = H5Fcreate(FILENAME3, H5F_ACC_TRUNC, fcpl, fapl3)) < 0)
        TEST_ERROR;
    if (H5Fclose(fid3) < 0)
        FAIL_STACK_ERROR;

    /* Open file 2 as VFD SWMR writer */
    if ((fid2 = H5Fopen(FILENAME2, H5F_ACC_RDWR, fapl2)) < 0)
        TEST_ERROR;

    /* Open file 3 as VFD SWMR writer */
    if ((fid3 = H5Fopen(FILENAME3, H5F_ACC_RDWR, fapl3)) < 0)
        TEST_ERROR;

    /* Get file pointer for the 3 files */
    f1 = H5VL_object(fid1);
    f2 = H5VL_object(fid2);
    f3 = H5VL_object(fid3);

    /* Saved tick_num for the 3 files */
    s1 = f1->shared->tick_num;
    s2 = f2->shared->tick_num;
    s3 = f3->shared->tick_num;

    /* Trigger EOT for file 2 */
    if (H5Fvfd_swmr_end_tick(fid2) < 0)
        TEST_ERROR;

    /* file 2: tick_num should increase or at least same as previous tick_num */
    if (f2->shared->tick_num < s2)
        TEST_ERROR;

    /* Disable EOT for file 2 */
    if (H5Fvfd_swmr_disable_end_of_tick(fid2) < 0)
        TEST_ERROR;

    /* Should fail to trigger end of tick processing for file 2 */
    H5E_BEGIN_TRY
    {
        ret = H5Fvfd_swmr_end_tick(fid2);
    }
    H5E_END_TRY;
    if (ret >= 0)
        TEST_ERROR;

    if (H5Fclose(fid2) < 0)
        FAIL_STACK_ERROR;

    /* Trigger EOT for file 1 */
    if (H5Fvfd_swmr_end_tick(fid1) < 0)
        TEST_ERROR;

    /* file 1: tick_num should increase or at least same as previous tick_num */
    if (f1->shared->tick_num < s1)
        TEST_ERROR;

    if (H5Fclose(fid1) < 0)
        FAIL_STACK_ERROR;

    /* Trigger EOT for file 3 */
    if (H5Fvfd_swmr_end_tick(fid3) < 0)
        TEST_ERROR;

    /* file 3: tick_num should increase or at least same as previous tick_num */
    if (f3->shared->tick_num < s3)
        TEST_ERROR;

    if (H5Fclose(fid3) < 0)
        FAIL_STACK_ERROR;

    /* Closing */
    if (H5Pclose(fapl1) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(fapl2) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(fapl3) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(fcpl) < 0)
        FAIL_STACK_ERROR;

    /* Free buffers */
    if (config1)
        HDfree(config1);
    if (config2)
        HDfree(config2);
    if (config3)
        HDfree(config3);

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(fapl1);
        H5Pclose(fapl2);
        H5Pclose(fapl3);
        H5Pclose(fcpl);
        H5Fclose(fid1);
        H5Fclose(fid2);
        H5Fclose(fid3);
    }
    H5E_END_TRY;

    if (config1)
        HDfree(config1);
    if (config2)
        HDfree(config2);
    if (config3)
        HDfree(config3);

    return 1;
} /* test_file_end_tick() */

/*-------------------------------------------------------------------------
 * Function:    test_writer_create_open_flush()
 *
 * Purpose:     Verify info in the metadata file when:
 *              --creating the HDF5 file
 *              --flushing the HDF5 file
 *              --opening an existing HDF5 file
 *              It will call the internal testing routine
 *              H5F__vfd_swmr_writer_create_open_flush_test() to do the following:
 *              --Open the metadata file
 *              --Verify the file size is as expected (md_pages_reserved)
 *              --For file create:
 *                  --No header magic is found
 *              --For file open or file flush:
 *                  --Read and decode the header and index in the metadata file
 *                  --Verify info in the header and index read from
 *                    the metadata file is as expected (empty index)
 *
 * Return:      0 if test is sucessful
 *              1 if test fails
 *
 * Programmer:  Vailin Choi; October 2018
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_writer_create_open_flush(void)
{
    hid_t                  fid       = -1;   /* File ID */
    hid_t                  fapl      = -1;   /* File access property list */
    hid_t                  fcpl      = -1;   /* File creation property list */
    H5F_vfd_swmr_config_t *my_config = NULL; /* Configuration for VFD SWMR */

    TESTING("Create/Open/Flush an HDF5 file for VFD SWMR");

    /* Allocate memory for the configuration structure */
    if ((my_config = HDmalloc(sizeof(H5F_vfd_swmr_config_t))) == NULL)
        FAIL_STACK_ERROR;

    /*
     * Set up the VFD SWMR configuration + page buffering
     */

    /* config, tick_len, max_lag, writer, maintain_metadata_file, generate_updater_files,
       flush_raw_data, md_pages_reserved, md_file_path, updater_file_path */
    init_vfd_swmr_config(my_config, 1, 3, TRUE, TRUE, FALSE, TRUE, 2, MD_FILENAME, NULL);

    /* use_latest_format, use_vfd_swmr, only_meta_page, page_buf_size, config */
    fapl = vfd_swmr_create_fapl(FALSE, TRUE, FALSE, 4096, my_config);
    if (fapl == H5I_INVALID_HID)
        TEST_ERROR;

    if ((fcpl = vfd_swmr_create_fcpl(H5F_FSPACE_STRATEGY_PAGE, 4096)) < 0) {
        HDprintf("vfd_swmr_create_fcpl() failed");
        FAIL_STACK_ERROR;
    }

    /* Create an HDF5 file with VFD SWMR configured */
    if ((fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Verify info in metadata file when creating the HDF5 file */
    if (H5F__vfd_swmr_writer_create_open_flush_test(fid, TRUE) < 0)
        FAIL_STACK_ERROR;

    /* Flush the HDF5 file */
    if (H5Fflush(fid, H5F_SCOPE_GLOBAL) < 0)
        FAIL_STACK_ERROR;

    /* Verify info in metadata file when flushing the HDF5 file */
    if (H5F__vfd_swmr_writer_create_open_flush_test(fid, FALSE) < 0)
        FAIL_STACK_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Re-open the file as VFD SWMR writer */
    if ((fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Verify info in metadata file when reopening the HDF5 file */
    if (H5F__vfd_swmr_writer_create_open_flush_test(fid, FALSE) < 0)
        FAIL_STACK_ERROR;

    /* Closing */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(fcpl) < 0)
        FAIL_STACK_ERROR;

    if (my_config)
        HDfree(my_config);

    PASSED();
    return 0;

error:
    if (my_config)
        HDfree(my_config);

    H5E_BEGIN_TRY
    {
        H5Pclose(fapl);
        H5Pclose(fcpl);
        H5Fclose(fid);
    }
    H5E_END_TRY;

    return 1;
} /* test_writer_create_open_flush() */

/*-------------------------------------------------------------------------
 * Function:    test_writer_md()
 *
 * Purpose:     Verify info in the metadata file after updating with the
 *              constructed index: (A), (B), (C), (D)
 *              It will call the internal testing routine
 *              H5F__vfd_swmr_writer_md_test() to do the following:
 *              --Update the metadata file with the input index via the
 *                internal library routine H5F_update_vfd_swmr_metadata_file()
 *              --Verify the entries in the delayed list is as expected:
 *                --num_dl_entries
 *              --Open the metadata file, read and decode the header and index
 *              --Verify header and index info just read from the metadata
 *                file is as expected:
 *                --num_entries and index
 *
 * Return:      0 if test is sucessful
 *              1 if test fails
 *
 * Programmer:  Vailin Choi; October 2018
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_writer_md(void)
{
    hid_t          fid         = -1;                                   /* File ID */
    hid_t          fapl        = -1;                                   /* File access property list */
    hid_t          fcpl        = -1;                                   /* File creation property list */
    const unsigned num_entries = 10;                                   /* index size */
    unsigned       i           = 0;                                    /* Local index variables */
    uint8_t *      buf         = NULL;                                 /* Data page from the page buffer */
    hid_t          dcpl        = -1;                                   /* Dataset creation property list */
    hid_t          sid         = -1;                                   /* Dataspace ID */
    hid_t          did         = -1;                                   /* Dataset ID */
    int *          rwbuf       = NULL;                                 /* Data buffer for writing */
    H5O_info2_t    oinfo;                                              /* Object metadata information */
    char           dname[100];                                         /* Name of dataset */
    hsize_t        dims[2]           = {50, 20};                       /* Dataset dimension sizes */
    hsize_t        max_dims[2]       = {H5S_UNLIMITED, H5S_UNLIMITED}; /* Dataset maximum dimension sizes */
    hsize_t        chunk_dims[2]     = {2, 5};                         /* Dataset chunked dimension sizes */
    H5FD_vfd_swmr_idx_entry_t *index = NULL;                           /* Pointer to the index entries */
    H5F_vfd_swmr_config_t *    my_config = NULL;                       /* Configuration for VFD SWMR */
    H5F_t *                    f         = NULL;                       /* Internal file object pointer */

    TESTING("Verify the metadata file for VFD SWMR writer");

    /* Allocate memory for the configuration structure */
    if ((my_config = HDmalloc(sizeof(H5F_vfd_swmr_config_t))) == NULL)
        FAIL_STACK_ERROR;

    /* config, tick_len, max_lag, writer, maintain_metadata_file, generate_updater_files,
       flush_raw_data, md_pages_reserved, md_file_path, updater_file_path */
    init_vfd_swmr_config(my_config, 1, 3, TRUE, TRUE, FALSE, TRUE, 256, MD_FILENAME, NULL);

    /* use_latest_format, use_vfd_swmr, only_meta_page, page_buf_size, config */
    fapl = vfd_swmr_create_fapl(FALSE, TRUE, FALSE, FS_PAGE_SIZE, my_config);
    if (fapl == H5I_INVALID_HID)
        TEST_ERROR;

    if ((fcpl = vfd_swmr_create_fcpl(H5F_FSPACE_STRATEGY_PAGE, FS_PAGE_SIZE)) < 0) {
        HDprintf("vfd_swmr_create_fcpl() failed");
        FAIL_STACK_ERROR;
    }

    /* Create an HDF5 file with VFD SWMR configured */
    if ((fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Get a pointer to the internal file object */
    if (NULL == (f = (H5F_t *)H5VL_object(fid)))
        FAIL_STACK_ERROR;

    /* Allocate num_entries for the data buffer */
    if ((buf = HDcalloc(num_entries, FS_PAGE_SIZE)) == NULL)
        FAIL_STACK_ERROR;

    /* Allocate memory for num_entries index */
    index = HDcalloc(num_entries, sizeof(H5FD_vfd_swmr_idx_entry_t));
    if (NULL == index)
        FAIL_STACK_ERROR;

    /* (A) Construct index for updating the metadata file */
    for (i = 0; i < num_entries; i++) {
        index[i].hdf5_page_offset    = 3 + 7 * i;
        index[i].md_file_page_offset = 1 + (num_entries - i) * 5;
        index[i].length              = (uint32_t)FS_PAGE_SIZE;
        index[i].entry_ptr           = &buf[i * FS_PAGE_SIZE];
        index[i].tick_of_last_change = f->shared->tick_num;
    }

    /* Update with index and verify info in the metadata file */
    /* Also verify that 0 entries will be on the delayed list */
    if (H5F__vfd_swmr_writer_md_test(fid, num_entries, index, 0) < 0)
        TEST_ERROR;

    /* Create dataset creation property list */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR;

    /* Set to use chunked dataset */
    if (H5Pset_chunk(dcpl, 2, chunk_dims) < 0)
        FAIL_STACK_ERROR;

    /* Create dataspace */
    if ((sid = H5Screate_simple(2, dims, max_dims)) < 0)
        FAIL_STACK_ERROR;

    /* Perform activities to ensure that max_lag ticks elapse */
    for (i = 0; i < my_config->max_lag + 1; i++) {
        decisleep(my_config->tick_len);

        /* Create a chunked dataset */
        HDsprintf(dname, "dset %d", i);
        if ((did = H5Dcreate2(fid, dname, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR;

        /* Get dataset object header address */
        if (H5Oget_info3(did, &oinfo, H5O_INFO_BASIC) < 0)
            FAIL_STACK_ERROR;

        /* Close the dataset */
        if (H5Dclose(did) < 0)
            FAIL_STACK_ERROR;
    }

    /* (B) Update every other entry in the index */
    for (i = 0; i < num_entries; i += 2) {
        index[i].entry_ptr           = &buf[i * FS_PAGE_SIZE];
        index[i].tick_of_last_change = f->shared->tick_num;
    }

    /* Update with index and verify info in the metadata file */
    /* Also verify that 5 entries will be on the delayed list */
    if (H5F__vfd_swmr_writer_md_test(fid, num_entries, index, 5) < 0)
        TEST_ERROR;

    /* Allocate memory for the read/write buffer */
    if ((rwbuf = HDmalloc(sizeof(*rwbuf) * (50 * 20))) == NULL)
        FAIL_STACK_ERROR;
    for (i = 0; i < (50 * 20); i++)
        rwbuf[i] = (int)i;

    /* Perform activities to ensure that max_lag ticks elapse */
    for (i = 0; i < my_config->max_lag + 1; i++) {
        decisleep(my_config->tick_len);

        /* Open the dataset */
        HDsprintf(dname, "dset %d", i);
        if ((did = H5Dopen2(fid, dname, H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR;

        /* Write to the dataset */
        if (H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rwbuf) < 0)
            FAIL_STACK_ERROR;

        /* Get dataset object info */
        if (H5Oget_info3(did, &oinfo, H5O_INFO_BASIC) < 0)
            FAIL_STACK_ERROR;

        /* Close the dataset */
        if (H5Dclose(did) < 0)
            FAIL_STACK_ERROR;
    }

    /* (C) Update every 3 entry in the index */
    for (i = 0; i < num_entries; i += 3) {
        index[i].entry_ptr           = &buf[i * FS_PAGE_SIZE];
        index[i].tick_of_last_change = f->shared->tick_num;
    }

    /* Update with index and verify info in the metadata file */
    /* Also verify that 4 entries will be on the delayed list */
    if (H5F__vfd_swmr_writer_md_test(fid, num_entries, index, 4) < 0)
        TEST_ERROR;

    /* Clear the read/write buffer */
    HDmemset(rwbuf, 0, sizeof(sizeof(int) * (50 * 20)));

    /* Perform activities to ensure that max_lag ticks elapse */
    for (i = 0; i < my_config->max_lag + 1; i++) {
        decisleep(my_config->tick_len);

        /* Open the dataset */
        HDsprintf(dname, "dset %d", i);
        if ((did = H5Dopen2(fid, dname, H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR;

        /* Read from the dataset */
        if (H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rwbuf) < 0)
            FAIL_STACK_ERROR;

        /* Get dataset object info */
        if (H5Oget_info3(did, &oinfo, H5O_INFO_BASIC) < 0)
            FAIL_STACK_ERROR;

        /* Close the dataset */
        if (H5Dclose(did) < 0)
            FAIL_STACK_ERROR;
    }

    /* (D) Update two entries in the index */
    index[1].entry_ptr           = &buf[1 * FS_PAGE_SIZE];
    index[1].tick_of_last_change = f->shared->tick_num;
    index[5].entry_ptr           = &buf[5 * FS_PAGE_SIZE];
    index[5].tick_of_last_change = f->shared->tick_num;

    /* Update with index and verify info in the metadata file */
    /* Also verify that 2 entries will be on the delayed list */
    if (H5F__vfd_swmr_writer_md_test(fid, num_entries, index, 2) < 0)
        TEST_ERROR;

    /* Closing */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;
    if (H5Sclose(sid) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(dcpl) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(fcpl) < 0)
        FAIL_STACK_ERROR;

    /* Free resources */
    if (my_config)
        HDfree(my_config);
    if (buf)
        HDfree(buf);
    if (rwbuf)
        HDfree(rwbuf);
    if (index)
        HDfree(index);

    PASSED();
    return 0;

error:
    if (my_config)
        HDfree(my_config);
    if (buf)
        HDfree(buf);
    if (rwbuf)
        HDfree(rwbuf);
    if (index)
        HDfree(index);

    H5E_BEGIN_TRY
    {
        H5Dclose(did);
        H5Sclose(sid);
        H5Pclose(dcpl);
        H5Pclose(fapl);
        H5Pclose(fcpl);
        H5Fclose(fid);
    }
    H5E_END_TRY;

    return 1;
} /* test_writer__md() */

#ifndef H5_HAVE_UNISTD_H

static unsigned
test_reader_md_concur(void)
{
    /* Output message about test being performed */
    TESTING("Verify the metadata file for VFD SWMR reader");
    SKIPPED();
    HDputs("    Test skipped (unistd.h not present)");
    return 0;

} /* test_reader_md_concur() */

static unsigned
test_multiple_file_opens_concur(void)
{
    /* Output message about test being performed */
    TESTING("EOT queue entries when opening files concurrently with VFD SWMR");
    SKIPPED();
    HDputs("    Test skipped (unistd.h not present)");
    return 0;

} /* test_multiple_file_opens_concur() */

static unsigned
test_disable_enable_eot_concur(void)
{
    /* Output message about test being performed */
    TESTING("Verify concurrent H5Fvfd_swmr_enable/disable_end_of_tick()");
    SKIPPED();
    HDputs("    Test skipped (unistd.h not present)");
    return 0;

} /* test_disable_enble_eot_concur() */

static unsigned
test_file_end_tick_concur(void)
{
    /* Output message about test being performed */
    TESTING("Verify concurrent H5Fvfd_swmr_end_tick()");
    SKIPPED();
    HDputs("    Test skipped (unistd.h not present)");
    return 0;

} /* test_disable_enble_eot_concur() */

#else /* H5_HAVE_UNISTD_H */

/*-------------------------------------------------------------------------
 * Function:    test_reader_md_concur()
 *
 * Purpose:     Verify metadata file info updated by the writer is
 *              what the reader obtained from the metadata file:
 *              --Cases (A), (B), (C), (D), (E)
 *              NOTE: Changes for page buffering/cache are not in place yet.
 *                    Index entries are constructed at the front end by the
 *                    writer and verified at the back end by the reader.
 *
 * Return:      0 if test is sucessful
 *              1 if test fails
 *
 * Programmer:  Vailin Choi; October 2018
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_reader_md_concur(void)
{
    unsigned    i     = 0;              /* Local index variables */
    uint8_t *   buf   = NULL;           /* Data page from the page buffer */
    hid_t       dcpl  = -1;             /* Dataset creation property list */
    hid_t       sid   = -1;             /* Dataspace ID */
    hid_t       did   = -1;             /* Dataset ID */
    int *       rwbuf = NULL;           /* Data buffer for writing */
    H5O_info2_t oinfo;                  /* Object metadata information */
    char        dname[100];             /* Name of dataset */
    hsize_t     dims[2]     = {50, 20}; /* Dataset dimension sizes */
    hsize_t     max_dims[2] =           /* Dataset maximum dimension sizes */
        {H5S_UNLIMITED, H5S_UNLIMITED};
    hsize_t                    chunk_dims[2] = {2, 5}; /* Dataset chunked dimension sizes */
    unsigned                   num_entries   = 0;      /* Number of entries in the index */
    H5FD_vfd_swmr_idx_entry_t *index         = NULL;   /* Pointer to the index entries */

    hid_t                  fcpl          = -1;    /* File creation property list */
    hid_t                  fid_writer    = -1;    /* File ID for writer */
    hid_t                  fapl_writer   = -1;    /* File access property list for writer */
    H5F_vfd_swmr_config_t *config_writer = NULL;  /* VFD SWMR Configuration for writer */
    pid_t                  tmppid;                /* Child process ID returned by waitpid */
    pid_t                  childpid = 0;          /* Child process ID */
    int                    child_status;          /* Status passed to waitpid */
    int                    child_wait_option = 0; /* Options passed to waitpid */
    int                    child_exit_val;        /* Exit status of the child */

    int    parent_pfd[2]; /* Pipe for parent process as writer */
    int    child_pfd[2];  /* Pipe for child process as reader */
    int    notify = 0;    /* Notification between parent and child */
    H5F_t *file_writer;   /* File pointer for writer */

    TESTING("Verify the metadata file for VFD SWMR reader");

    /* Allocate memory for the configuration structure */
    if ((config_writer = HDmalloc(sizeof(*config_writer))) == NULL)
        FAIL_STACK_ERROR;

    /*
     * Set up the VFD SWMR configuration + page buffering
     */

    /* config, tick_len, max_lag, writer, maintain_metadata_file, generate_updater_files,
       flush_raw_data, md_pages_reserved, md_file_path, updater_file_path */
    init_vfd_swmr_config(config_writer, 1, 3, TRUE, TRUE, FALSE, TRUE, 256, MD_FILENAME, NULL);

    /* use_latest_format, use_vfd_swmr, only_meta_page, page_buf_size, config */
    fapl_writer = vfd_swmr_create_fapl(FALSE, TRUE, FALSE, FS_PAGE_SIZE, config_writer);
    if (fapl_writer == H5I_INVALID_HID)
        TEST_ERROR;

    if ((fcpl = vfd_swmr_create_fcpl(H5F_FSPACE_STRATEGY_PAGE, FS_PAGE_SIZE)) < 0) {
        HDprintf("vfd_swmr_create_fcpl() failed");
        FAIL_STACK_ERROR;
    }

    /* Create an HDF5 file with VFD SWMR configured */
    if ((fid_writer = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, fapl_writer)) < 0)
        FAIL_STACK_ERROR;

    /* Close the file */
    if (H5Fclose(fid_writer) < 0)
        FAIL_STACK_ERROR;

    /* Create 2 pipes */
    if (HDpipe(parent_pfd) < 0)
        FAIL_STACK_ERROR;

    if (HDpipe(child_pfd) < 0)
        FAIL_STACK_ERROR;

    /* Fork child process */
    if ((childpid = HDfork()) < 0)
        FAIL_STACK_ERROR;

    /*
     * Child process as reader
     */
    if (childpid == 0) {
        int                        child_notify = 0;         /* Notification between child and parent */
        hid_t                      fid_reader   = -1;        /* File ID for reader */
        hid_t                      fapl_reader  = -1;        /* File access property list for reader */
        H5F_t *                    file_reader;              /* File pointer for reader */
        H5F_vfd_swmr_config_t *    config_reader     = NULL; /* VFD SWMR configuration for reader */
        unsigned                   child_num_entries = 0;    /* Number of entries passed to reader */
        H5FD_vfd_swmr_idx_entry_t *child_index       = NULL; /* Index passed to reader */

        /* Close unused write end for writer pipe */
        if (HDclose(parent_pfd[1]) < 0)
            HDexit(EXIT_FAILURE);

        /* Close unused read end for reader pipe */
        if (HDclose(child_pfd[0]) < 0)
            HDexit(EXIT_FAILURE);

        /* Free unused configuration */
        if (config_writer)
            HDfree(config_writer);

        /*
         * Case A: reader
         *  --verify an empty index
         */

        /* Wait for notification 1 from parent to start verification */
        while (child_notify != 1) {
            if (HDread(parent_pfd[0], &child_notify, sizeof(int)) < 0)
                HDexit(EXIT_FAILURE);
        }

        /* Allocate memory for the configuration structure */
        if ((config_reader = HDmalloc(sizeof(*config_reader))) == NULL)
            HDexit(EXIT_FAILURE);

        /*
         * Set up the VFD SWMR configuration as reader + page buffering
         */

        /* config, tick_len, max_lag, writer, maintain_metadata_file, generate_updater_files,
        flush_raw_data, md_pages_reserved, md_file_path, updater_file_path */
        init_vfd_swmr_config(config_reader, 1, 3, FALSE, TRUE, FALSE, TRUE, 256, MD_FILENAME, NULL);

        /* use_latest_format, use_vfd_swmr, only_meta_page, page_buf_size, config */
        fapl_reader = vfd_swmr_create_fapl(FALSE, TRUE, FALSE, FS_PAGE_SIZE, config_reader);

        if (fapl_reader == H5I_INVALID_HID)
            TEST_ERROR;

        /* Open the test file as reader */
        if ((fid_reader = H5Fopen(FILENAME, H5F_ACC_RDONLY, fapl_reader)) < 0)
            HDexit(EXIT_FAILURE);

        /* Get file pointer */
        file_reader = H5VL_object(fid_reader);

        /* Read and verify header and an empty index in the metadata file */
        if (H5FD__vfd_swmr_reader_md_test(file_reader->shared->lf, 0, NULL) < 0)
            HDexit(EXIT_FAILURE);

        /* Send notification 2 to parent that the verification is complete */
        child_notify = 2;
        if (HDwrite(child_pfd[1], &child_notify, sizeof(int)) < 0)
            HDexit(EXIT_FAILURE);

        /*
         * Case B: reader
         * --verify index as sent from writer
         */

        /* Wait for notification 3 from parent to start verification */
        while (child_notify != 3) {
            if (HDread(parent_pfd[0], &child_notify, sizeof(int)) < 0)
                HDexit(EXIT_FAILURE);
        }

        /* Read num_entries from writer pipe */
        if (HDread(parent_pfd[0], &child_num_entries, sizeof(int)) < 0)
            HDexit(EXIT_FAILURE);

        /* Free previous index */
        if (child_index)
            HDfree(child_index);

        if (child_num_entries) {

            /* Allocate memory for num_entries index */
            if ((child_index = HDcalloc(child_num_entries, sizeof(*child_index))) == NULL)
                HDexit(EXIT_FAILURE);

            /* Read index from writer pipe */
            if (HDread(parent_pfd[0], child_index, child_num_entries * sizeof(*child_index)) < 0)
                HDexit(EXIT_FAILURE);
        }

        /* Read and verify the expected header and index info in the
         * metadata file
         */
        if (H5FD__vfd_swmr_reader_md_test(file_reader->shared->lf, child_num_entries, child_index) < 0)
            HDexit(EXIT_FAILURE);

        /* Send notification 4 to parent that the verification is complete */
        child_notify = 4;
        if (HDwrite(child_pfd[1], &child_notify, sizeof(int)) < 0)
            HDexit(EXIT_FAILURE);

        /*
         * Case C: reader
         * --verify index as sent from writer
         */

        /* Wait for notification 5 from parent to start verification */
        while (child_notify != 5) {
            if (HDread(parent_pfd[0], &child_notify, sizeof(int)) < 0)
                HDexit(EXIT_FAILURE);
        }

        /* Read num_entries from writer pipe */
        if (HDread(parent_pfd[0], &child_num_entries, sizeof(int)) < 0)
            HDexit(EXIT_FAILURE);

        /* Free previous index */
        if (child_index)
            HDfree(child_index);

        if (child_num_entries) {
            /* Allocate memory for num_entries index */
            if ((child_index = (H5FD_vfd_swmr_idx_entry_t *)HDcalloc(
                     child_num_entries, sizeof(H5FD_vfd_swmr_idx_entry_t))) == NULL)
                HDexit(EXIT_FAILURE);

            /* Read index from writer pipe */
            if (HDread(parent_pfd[0], child_index, child_num_entries * sizeof(H5FD_vfd_swmr_idx_entry_t)) < 0)
                HDexit(EXIT_FAILURE);
        }

        /* Read and verify the expected header and index info in the
         * metadata file
         */
        if (H5FD__vfd_swmr_reader_md_test(file_reader->shared->lf, child_num_entries, child_index) < 0)
            HDexit(EXIT_FAILURE);

        /* Send notification 6 to parent that the verification is complete */
        child_notify = 6;
        if (HDwrite(child_pfd[1], &child_notify, sizeof(int)) < 0)
            HDexit(EXIT_FAILURE);

        /*
         * Case D: reader
         * --verify index as sent from writer
         */

        /* Wait for notification 7 from parent to start verification */
        while (child_notify != 7) {

            if (HDread(parent_pfd[0], &child_notify, sizeof(int)) < 0)

                HDexit(EXIT_FAILURE);
        }

        /* Read num_entries from writer pipe */
        if (HDread(parent_pfd[0], &child_num_entries, sizeof(int)) < 0)
            HDexit(EXIT_FAILURE);

        /* Free previous index */
        if (child_index)
            HDfree(child_index);

        if (child_num_entries) {
            /* Allocate memory for num_entries index */
            if ((child_index = (H5FD_vfd_swmr_idx_entry_t *)HDcalloc(
                     child_num_entries, sizeof(H5FD_vfd_swmr_idx_entry_t))) == NULL)
                HDexit(EXIT_FAILURE);

            /* Read index from writer pipe */
            if (HDread(parent_pfd[0], child_index, child_num_entries * sizeof(H5FD_vfd_swmr_idx_entry_t)) < 0)
                HDexit(EXIT_FAILURE);
        }

        /* Read and verify the expected header and index info in the
         * metadata file
         */
        if (H5FD__vfd_swmr_reader_md_test(file_reader->shared->lf, child_num_entries, child_index) < 0)
            HDexit(EXIT_FAILURE);

        /* Send notification 8 to parent that the verification is complete */
        child_notify = 8;
        if (HDwrite(child_pfd[1], &child_notify, sizeof(int)) < 0)
            HDexit(EXIT_FAILURE);

        /*
         * Case E: reader
         * --verify an empty index
         */

        /* Wait for notification 9 from parent to start verification */
        while (child_notify != 9) {
            if (HDread(parent_pfd[0], &child_notify, sizeof(int)) < 0)
                HDexit(EXIT_FAILURE);
        }

        /* Read and verify header and an empty index in the metadata file */
        if (H5FD__vfd_swmr_reader_md_test(file_reader->shared->lf, 0, NULL) < 0)
            HDexit(EXIT_FAILURE);

        /* Free resources */
        if (child_index)
            HDfree(child_index);
        if (config_reader)
            HDfree(config_reader);

        /* Closing */
        if (H5Fclose(fid_reader) < 0)
            HDexit(EXIT_FAILURE);
        if (H5Pclose(fapl_reader) < 0)
            HDexit(EXIT_FAILURE);

        /* Close the pipes */
        if (HDclose(parent_pfd[0]) < 0)
            HDexit(EXIT_FAILURE);
        if (HDclose(child_pfd[1]) < 0)
            HDexit(EXIT_FAILURE);

        HDexit(EXIT_SUCCESS);
    } /* end child process */

    /*
     * Parent process as writer
     */

    /* Close unused read end for writer pipe */
    if (HDclose(parent_pfd[0]) < 0)
        FAIL_STACK_ERROR;

    /* Close unused write end for reader pipe */
    if (HDclose(child_pfd[1]) < 0)
        FAIL_STACK_ERROR;

    /*
     * Case A: writer
     * --open the file as VFD SWMR writer
     */

    /* Open as VFD SWMR writer */
    if ((fid_writer = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl_writer)) < 0)
        FAIL_STACK_ERROR;

    /* Get the file pointer */
    file_writer = H5VL_object(fid_writer);

    /* Send notification 1 to reader to start verfication */
    notify = 1;
    if (HDwrite(parent_pfd[1], &notify, sizeof(int)) < 0)
        FAIL_STACK_ERROR;

    /*
     * Case B: writer
     *  --create datasets to ensure ticks elapse
     *  --construct 12 entries in the index
     *  --update the metadata file with the index
     */

    /* Wait for notification 2 from reader that the verifcation is complete */
    while (notify != 2) {
        if (HDread(child_pfd[0], &notify, sizeof(int)) < 0)
            FAIL_STACK_ERROR;
    }

    /* Create dataset creation property list */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR;

    /* Set to use chunked dataset */
    if (H5Pset_chunk(dcpl, 2, chunk_dims) < 0)
        FAIL_STACK_ERROR;

    /* Create dataspace */
    if ((sid = H5Screate_simple(2, dims, max_dims)) < 0)
        FAIL_STACK_ERROR;

    /* Perform activities to ensure that ticks elapse */
    for (i = 0; i < config_writer->max_lag + 1; i++) {
        decisleep(config_writer->tick_len);

        /* Create a chunked dataset */
        HDsprintf(dname, "dset %d", i);
        if ((did = H5Dcreate2(fid_writer, dname, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR;

        /* Get dataset object header address */
        if (H5Oget_info3(did, &oinfo, H5O_INFO_BASIC) < 0)
            FAIL_STACK_ERROR;

        /* Close the dataset */
        if (H5Dclose(did) < 0)
            FAIL_STACK_ERROR;
    }

    num_entries = 12;

    /* Allocate num_entries for the data buffer */
    if ((buf = HDcalloc(num_entries, FS_PAGE_SIZE)) == NULL)
        FAIL_STACK_ERROR;

    /* Allocate memory for num_entries index */
    index = HDcalloc(num_entries, sizeof(H5FD_vfd_swmr_idx_entry_t));
    if (NULL == index)
        FAIL_STACK_ERROR;

    /* Construct index for updating the metadata file */
    for (i = 0; i < num_entries; i++) {
        index[i].hdf5_page_offset    = 3 + 7 * i;
        index[i].md_file_page_offset = 1 + (num_entries - i) * 5;
        index[i].length              = (uint32_t)FS_PAGE_SIZE;
        index[i].entry_ptr           = &buf[i * FS_PAGE_SIZE];
        index[i].tick_of_last_change = file_writer->shared->tick_num;
    }

    /* Update the metadata file with the index */
    if (H5F_update_vfd_swmr_metadata_file(file_writer, num_entries, index) < 0)
        TEST_ERROR;

    /* Send notification 3 to child to start verification */
    notify = 3;
    if (HDwrite(parent_pfd[1], &notify, sizeof(int)) < 0)
        FAIL_STACK_ERROR;

    /* Send num_entries to the reader */
    if (HDwrite(parent_pfd[1], &num_entries, sizeof(int)) < 0)
        FAIL_STACK_ERROR;

    /* Send index to the reader */
    if (HDwrite(parent_pfd[1], index, num_entries * sizeof(H5FD_vfd_swmr_idx_entry_t)) < 0)
        FAIL_STACK_ERROR;

    /*
     * Case C: writer
     *  --write to the datasets to ensure ticks elapse
     *  --update 3 entries in the index
     *  --update the metadata file with the index
     */

    /* Wait for notification 4 from reader that the verifcation is complete */
    while (notify != 4) {
        if (HDread(child_pfd[0], &notify, sizeof(int)) < 0)
            FAIL_STACK_ERROR;
    }

    /* Allocate memory for the read/write buffer */
    if ((rwbuf = HDmalloc(sizeof(*rwbuf) * (50 * 20))) == NULL)
        FAIL_STACK_ERROR;
    for (i = 0; i < (50 * 20); i++)
        rwbuf[i] = (int)i;

    /* Perform activities to ensure that max_lag ticks elapse */
    for (i = 0; i < config_writer->max_lag + 1; i++) {
        decisleep(config_writer->tick_len);

        /* Open the dataset */
        HDsprintf(dname, "dset %d", i);
        if ((did = H5Dopen2(fid_writer, dname, H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR;

        /* Write to the dataset */
        if (H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rwbuf) < 0)
            FAIL_STACK_ERROR;

        /* Close the dataset */
        if (H5Dclose(did) < 0)
            FAIL_STACK_ERROR;
    }

    /* Update 3 entries in the index */
    num_entries = 3;
    for (i = 0; i < num_entries; i++) {
        index[i].entry_ptr           = &buf[i * FS_PAGE_SIZE];
        index[i].tick_of_last_change = file_writer->shared->tick_num;
    }

    /* Update the metadata file with the index */
    if (H5F_update_vfd_swmr_metadata_file(file_writer, num_entries, index) < 0)
        TEST_ERROR;

    /* Send notification 5 to reader to start verification */
    notify = 5;
    if (HDwrite(parent_pfd[1], &notify, sizeof(int)) < 0)
        FAIL_STACK_ERROR;

    /* Send num_entries to the reader */
    if (HDwrite(parent_pfd[1], &num_entries, sizeof(int)) < 0)
        FAIL_STACK_ERROR;

    /* Send index to the reader */
    if (HDwrite(parent_pfd[1], index, num_entries * sizeof(H5FD_vfd_swmr_idx_entry_t)) < 0)
        FAIL_STACK_ERROR;

    /*
     * Case D: writer
     *  --read from the datasets to ensure ticks elapse
     *  --update 5 entries in the index
     *  --update the metadata file with the index
     */

    /* Wait for notification 6 from reader that the verifcation is complete */
    while (notify != 6) {
        if (HDread(child_pfd[0], &notify, sizeof(int)) < 0)
            FAIL_STACK_ERROR;
    }

    /* Perform activities to ensure that max_lag ticks elapse */
    for (i = 0; i < config_writer->max_lag + 1; i++) {
        decisleep(config_writer->tick_len);

        /* Open the dataset */
        HDsprintf(dname, "dset %d", i);
        if ((did = H5Dopen2(fid_writer, dname, H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR;

        /* Read from the dataset */
        if (H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rwbuf) < 0)
            FAIL_STACK_ERROR;

        /* Close the dataset */
        if (H5Dclose(did) < 0)
            FAIL_STACK_ERROR;
    }

    /* Update 5 entries in the index */
    num_entries = 5;
    for (i = 0; i < num_entries; i++) {
        index[i].entry_ptr           = &buf[i * FS_PAGE_SIZE];
        index[i].tick_of_last_change = file_writer->shared->tick_num;
    }

    /* Update the metadata file with the index */
    if (H5F_update_vfd_swmr_metadata_file(file_writer, num_entries, index) < 0)
        TEST_ERROR;

    /* Send notification 7 to reader to start verification */
    notify = 7;
    if (HDwrite(parent_pfd[1], &notify, sizeof(int)) < 0)
        FAIL_STACK_ERROR;

    /* Send num_entries to the reader */
    if (HDwrite(parent_pfd[1], &num_entries, sizeof(int)) < 0)
        FAIL_STACK_ERROR;

    /* Send index to the reader */
    if (HDwrite(parent_pfd[1], index, num_entries * sizeof(H5FD_vfd_swmr_idx_entry_t)) < 0)
        FAIL_STACK_ERROR;

    /*
     * Case E: writer
     * --write to the datasets again to ensure ticks elapse
     * --update the metadata file with an empty index
     */

    /* Wait for notification 8 from reader that the verifcation is complete */
    while (notify != 8) {
        if (HDread(child_pfd[0], &notify, sizeof(int)) < 0)
            FAIL_STACK_ERROR;
    }

    /* Perform activities to ensure that ticks elapse */
    for (i = 0; i < config_writer->max_lag + 1; i++) {
        decisleep(config_writer->tick_len);

        /* Open the dataset */
        HDsprintf(dname, "dset %d", i);
        if ((did = H5Dopen2(fid_writer, dname, H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR;

        /* Write to the dataset */
        if (H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rwbuf) < 0)
            FAIL_STACK_ERROR;

        /* Close the dataset */
        if (H5Dclose(did) < 0)
            FAIL_STACK_ERROR;
    }

    /* Update the metadata file with 0 entries and NULL index */
    if (H5F_update_vfd_swmr_metadata_file(file_writer, 0, NULL) < 0)
        TEST_ERROR;

    /* Send notification 8 to reader to start verification */
    notify = 9;
    if (HDwrite(parent_pfd[1], &notify, sizeof(int)) < 0)
        FAIL_STACK_ERROR;

    /*
     * Done
     */

    /* Close the pipes */
    if (HDclose(parent_pfd[1]) < 0)
        FAIL_STACK_ERROR;
    if (HDclose(child_pfd[0]) < 0)
        FAIL_STACK_ERROR;

    /* Wait for child process to complete */
    if ((tmppid = HDwaitpid(childpid, &child_status, child_wait_option)) < 0)
        FAIL_STACK_ERROR;

    /* Check exit status of child process */
    if (WIFEXITED(child_status)) {
        if ((child_exit_val = WEXITSTATUS(child_status)) != 0)
            TEST_ERROR;
    }
    else { /* child process terminated abnormally */
        TEST_ERROR;
    }

    /* Closing */
    if (H5Fclose(fid_writer) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(fapl_writer) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(fcpl) < 0)
        FAIL_STACK_ERROR;

    /* Free resources */
    if (config_writer)
        HDfree(config_writer);
    if (buf)
        HDfree(buf);
    if (rwbuf)
        HDfree(rwbuf);
    if (index)
        HDfree(index);

    PASSED();
    return 0;

error:
    if (config_writer)
        HDfree(config_writer);
    if (buf)
        HDfree(buf);
    if (rwbuf)
        HDfree(rwbuf);
    if (index)
        HDfree(index);

    H5E_BEGIN_TRY
    {
        H5Pclose(fapl_writer);
        H5Fclose(fid_writer);
        H5Pclose(fcpl);
    }
    H5E_END_TRY;

    return 1;
} /* test_reader_md_concur() */

/*-------------------------------------------------------------------------
 * Function:    test_multiple_file_opens_concur()
 *
 * Purpose:     Verify the entries on the EOT queue when opening files
 *              with and without VFD SWMR configured.
 *
 * Return:      0 if test is sucessful
 *              1 if test fails
 *
 * Programmer:  Vailin Choi; 11/18/2019
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_multiple_file_opens_concur(void)
{
    hid_t                  fcpl = H5I_INVALID_HID;
    pid_t                  tmppid;                /* Child process ID returned by waitpid */
    pid_t                  childpid = 0;          /* Child process ID */
    int                    child_status;          /* Status passed to waitpid */
    int                    child_wait_option = 0; /* Options passed to waitpid */
    int                    child_exit_val;        /* Exit status of the child */
    int                    parent_pfd[2];         /* Pipe for parent process as writer */
    int                    child_pfd[2];          /* Pipe for child process as reader */
    int                    notify = 0;            /* Notification between parent and child */
    hid_t                  fid1 = H5I_INVALID_HID, fid2 = H5I_INVALID_HID;
    hid_t                  fapl1 = H5I_INVALID_HID, fapl2 = H5I_INVALID_HID;
    H5F_vfd_swmr_config_t *config1 = NULL; /* VFD SWMR configuration */
    H5F_vfd_swmr_config_t *config2 = NULL; /* VFD SWMR configuration */
    H5F_t *                f1, *f2;        /* File pointer */
    eot_queue_entry_t *    curr;

    TESTING("EOT queue entries when opening files concurrently with VFD SWMR");

    if ((fcpl = vfd_swmr_create_fcpl(H5F_FSPACE_STRATEGY_PAGE, FS_PAGE_SIZE)) < 0) {
        HDprintf("vfd_swmr_create_fcpl() failed");
        FAIL_STACK_ERROR;
    }

    /* Create file A */
    if ((fid1 = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Close the file */
    if (H5Fclose(fid1) < 0)
        FAIL_STACK_ERROR;

    /* Create file B */
    if ((fid2 = H5Fcreate(FILENAME2, H5F_ACC_TRUNC, fcpl, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Close the file */
    if (H5Fclose(fid2) < 0)
        FAIL_STACK_ERROR;

    /* Create 2 pipes */
    if (HDpipe(parent_pfd) < 0)
        FAIL_STACK_ERROR;

    if (HDpipe(child_pfd) < 0)
        FAIL_STACK_ERROR;

    /* Fork child process */
    if ((childpid = HDfork()) < 0)
        FAIL_STACK_ERROR;

    /*
     * Child process
     */
    if (childpid == 0) {
        int                    child_notify  = 0;    /* Notification between child and parent */
        hid_t                  fid_writer    = -1;   /* File ID for writer */
        hid_t                  fapl_writer   = -1;   /* File access property list for writer */
        H5F_vfd_swmr_config_t *config_writer = NULL; /* VFD SWMR configuration for reader */

        /* Close unused write end for writer pipe */
        if (HDclose(parent_pfd[1]) < 0)
            HDexit(EXIT_FAILURE);

        /* Close unused read end for reader pipe */
        if (HDclose(child_pfd[0]) < 0)
            HDexit(EXIT_FAILURE);

        /*
         * Set up and open file B as VFD SWMR writer
         */

        /* Wait for notification 1 from parent before opening file B */
        while (child_notify != 1) {
            if (HDread(parent_pfd[0], &child_notify, sizeof(int)) < 0)
                HDexit(EXIT_FAILURE);
        }

        /* Allocate memory for VFD SMWR configuration */
        if ((config_writer = HDmalloc(sizeof(*config_writer))) == NULL)
            HDexit(EXIT_FAILURE);

        /* Set the VFD SWMR configuration in fapl_writer + page buffering */

        /* config, tick_len, max_lag, writer, maintain_metadata_file, generate_updater_files,
        flush_raw_data, md_pages_reserved, md_file_path, updater_file_path */
        init_vfd_swmr_config(config_writer, 1, 3, TRUE, TRUE, FALSE, TRUE, 256, MD_FILENAME2, NULL);

        /* use_latest_format, use_vfd_swmr, only_meta_page, page_buf_size, config */
        fapl_writer = vfd_swmr_create_fapl(FALSE, TRUE, FALSE, FS_PAGE_SIZE, config_writer);

        if (fapl_writer == H5I_INVALID_HID)
            HDexit(EXIT_FAILURE);

        /* Open file B as VFD SWMR writer */
        if ((fid_writer = H5Fopen(FILENAME2, H5F_ACC_RDWR, fapl_writer)) < 0)
            HDexit(EXIT_FAILURE);

        /* Send notification 2 to parent that file B is open */
        child_notify = 2;
        if (HDwrite(child_pfd[1], &child_notify, sizeof(int)) < 0)
            HDexit(EXIT_FAILURE);

        /* Wait for notification 3 from parent before closing file B */
        while (child_notify != 3) {
            if (HDread(parent_pfd[0], &child_notify, sizeof(int)) < 0)
                HDexit(EXIT_FAILURE);
        }

        if (config_writer)
            HDfree(config_writer);

        /* Close the file */
        if (H5Fclose(fid_writer) < 0)
            HDexit(EXIT_FAILURE);
        if (H5Pclose(fapl_writer) < 0)
            HDexit(EXIT_FAILURE);

        /* Send notification 4 to parent that file B is closed */
        child_notify = 4;
        if (HDwrite(child_pfd[1], &child_notify, sizeof(int)) < 0)
            HDexit(EXIT_FAILURE);

        /* Close the pipes */
        if (HDclose(parent_pfd[0]) < 0)
            HDexit(EXIT_FAILURE);
        if (HDclose(child_pfd[1]) < 0)
            HDexit(EXIT_FAILURE);

        HDexit(EXIT_SUCCESS);
    } /* end child process */

    /*
     * Parent process
     */

    /* Close unused read end for writer pipe */
    if (HDclose(parent_pfd[0]) < 0)
        FAIL_STACK_ERROR;

    /* Close unused write end for reader pipe */
    if (HDclose(child_pfd[1]) < 0)
        FAIL_STACK_ERROR;

    /*
     * Set up and open file A as VFD SWMR writer
     */

    /* Allocate memory for VFD SWMR configuration */
    if ((config1 = HDmalloc(sizeof(*config1))) == NULL)
        FAIL_STACK_ERROR;

    /* config, tick_len, max_lag, writer, maintain_metadata_file, generate_updater_files,
    flush_raw_data, md_pages_reserved, md_file_path, updater_file_path */
    init_vfd_swmr_config(config1, 7, 10, TRUE, TRUE, FALSE, TRUE, 256, MD_FILENAME, NULL);

    /* use_latest_format, use_vfd_swmr, only_meta_page, page_buf_size, config */
    fapl1 = vfd_swmr_create_fapl(FALSE, TRUE, FALSE, FS_PAGE_SIZE, config1);

    if (fapl1 == H5I_INVALID_HID)
        FAIL_STACK_ERROR;

    /* Open file A as VFD SWMR writer */
    if ((fid1 = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl1)) < 0)
        FAIL_STACK_ERROR;

    /* Get a pointer to the internal file object */
    if (NULL == (f1 = H5VL_object(fid1)))
        FAIL_STACK_ERROR;

    /* Head of EOT queue should be a writer */
    if ((curr = TAILQ_FIRST(&eot_queue_g)) == NULL || !curr->vfd_swmr_writer)
        TEST_ERROR;

    /* The EOT queue's first entry should be f1 */
    if ((curr = TAILQ_FIRST(&eot_queue_g)) == NULL || curr->vfd_swmr_file != f1)
        TEST_ERROR;

    /* Send notification 1 to child to open file B */
    notify = 1;
    if (HDwrite(parent_pfd[1], &notify, sizeof(int)) < 0)
        FAIL_STACK_ERROR;

    /* Wait for notification 2 from child that file B is open */
    while (notify != 2) {
        if (HDread(child_pfd[0], &notify, sizeof(int)) < 0)
            FAIL_STACK_ERROR;
    }

    /* Open file B as VFD SWMR reader */

    /* Allocate memory for VFD SWMR configuration */
    if ((config2 = HDmalloc(sizeof(*config2))) == NULL)
        FAIL_STACK_ERROR;

    /* config, tick_len, max_lag, writer, maintain_metadata_file, generate_updater_files,
    flush_raw_data, md_pages_reserved, md_file_path, updater_file_path */
    init_vfd_swmr_config(config2, 1, 3, FALSE, TRUE, FALSE, TRUE, 256, MD_FILENAME2, NULL);

    /* use_latest_format, use_vfd_swmr, only_meta_page, page_buf_size, config */
    fapl2 = vfd_swmr_create_fapl(FALSE, TRUE, FALSE, FS_PAGE_SIZE, config2);

    if (fapl2 == H5I_INVALID_HID)
        FAIL_STACK_ERROR;

    /* Open file B as VFD SWMR reader */
    if ((fid2 = H5Fopen(FILENAME2, H5F_ACC_RDONLY, fapl2)) < 0)
        FAIL_STACK_ERROR;

    /* Get a pointer to the internal file object */
    if (NULL == (f2 = H5VL_object(fid2)))
        FAIL_STACK_ERROR;

    /* Head of EOT queue should NOT be a writer */
    if ((curr = TAILQ_FIRST(&eot_queue_g)) != NULL && curr->vfd_swmr_writer)
        TEST_ERROR;

    /* The EOT queue's first entry should be f2 */
    if ((curr = TAILQ_FIRST(&eot_queue_g)) == NULL || curr->vfd_swmr_file != f2)
        TEST_ERROR;

    /* Send notification 3 to child to close file B */
    notify = 3;
    if (HDwrite(parent_pfd[1], &notify, sizeof(int)) < 0)
        FAIL_STACK_ERROR;

    /* Wait for notification 4 from child that file B is closed */
    while (notify != 4) {
        if (HDread(child_pfd[0], &notify, sizeof(int)) < 0)
            FAIL_STACK_ERROR;
    }

    /*
     * Done
     */

    /* Close the pipes */
    if (HDclose(parent_pfd[1]) < 0)
        FAIL_STACK_ERROR;
    if (HDclose(child_pfd[0]) < 0)
        FAIL_STACK_ERROR;

    /* Wait for child process to complete */
    if ((tmppid = HDwaitpid(childpid, &child_status, child_wait_option)) < 0)
        FAIL_STACK_ERROR;

    /* Check exit status of child process */
    if (WIFEXITED(child_status)) {
        if ((child_exit_val = WEXITSTATUS(child_status)) != 0)
            TEST_ERROR;
    }
    else { /* child process terminated abnormally */
        TEST_ERROR;
    }

    /* Closing */
    if (H5Fclose(fid1) < 0)
        FAIL_STACK_ERROR;
    if (H5Fclose(fid2) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(fapl1) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(fapl2) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(fcpl) < 0)
        FAIL_STACK_ERROR;

    /* Free resources */
    if (config1)
        HDfree(config1);
    if (config2)
        HDfree(config2);

    PASSED();
    return 0;

error:
    if (config1)
        HDfree(config1);
    if (config2)
        HDfree(config2);

    H5E_BEGIN_TRY
    {
        H5Pclose(fapl1);
        H5Pclose(fapl2);
        H5Fclose(fid1);
        H5Fclose(fid2);
        H5Pclose(fcpl);
    }
    H5E_END_TRY;

    return 1;
} /* test_multiple_file_opens_concur() */

/*-------------------------------------------------------------------------
 * Function:    test_enable_disable_eot_concur()
 *
 * Purpose:     Verify the public routines:
 *                  H5Fvfd_swmr_disable_end_of_tick()
 *                  H5Fvfd_swmr_enable_end_of_tick()
 *              enables/disables EOT when the files are opened
 *              concurrently.
 *
 * Return:      0 if test is sucessful
 *              1 if test fails
 *
 * Programmer:  Vailin Choi; June 2020
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_disable_enable_eot_concur(void)
{
    hid_t                  fcpl          = -1;    /* File creation property list */
    hid_t                  fid_writer    = -1;    /* File ID for writer */
    hid_t                  fapl_writer   = -1;    /* File access property list for writer */
    H5F_vfd_swmr_config_t *config_writer = NULL;  /* VFD SWMR Configuration for writer */
    pid_t                  tmppid;                /* Child process ID returned by waitpid */
    pid_t                  childpid = 0;          /* Child process ID */
    int                    child_status;          /* Status passed to waitpid */
    int                    child_wait_option = 0; /* Options passed to waitpid */
    int                    child_exit_val;        /* Exit status of the child */

    int parent_pfd[2]; /* Pipe for parent process as writer */
    int child_pfd[2];  /* Pipe for child process as reader */
    int notify = 0;    /* Notification between parent and child */

    TESTING("Verify concurrent H5Fvfd_swmr_enable/disable_end_of_tick()");

    /* Allocate memory for the configuration structure */
    if ((config_writer = HDmalloc(sizeof(*config_writer))) == NULL)
        FAIL_STACK_ERROR;

    /*
     * Set up the VFD SWMR configuration + page buffering
     */

    /* config, tick_len, max_lag, writer, maintain_metadata_file, generate_updater_files,
    flush_raw_data, md_pages_reserved, md_file_path, updater_file_path */
    init_vfd_swmr_config(config_writer, 1, 3, TRUE, TRUE, FALSE, TRUE, 256, MD_FILENAME, NULL);

    /* use_latest_format, use_vfd_swmr, only_meta_page, page_buf_size, config */
    fapl_writer = vfd_swmr_create_fapl(FALSE, TRUE, FALSE, FS_PAGE_SIZE, config_writer);

    if (fapl_writer == H5I_INVALID_HID)
        FAIL_STACK_ERROR;

    if ((fcpl = vfd_swmr_create_fcpl(H5F_FSPACE_STRATEGY_PAGE, FS_PAGE_SIZE)) < 0) {
        HDprintf("vfd_swmr_create_fcpl() failed");
        FAIL_STACK_ERROR;
    }

    /* Create an HDF5 file with VFD SWMR configured */
    if ((fid_writer = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, fapl_writer)) < 0)
        FAIL_STACK_ERROR;

    /* Close the file */
    if (H5Fclose(fid_writer) < 0)
        FAIL_STACK_ERROR;

    /* Create 2 pipes */
    if (HDpipe(parent_pfd) < 0)
        FAIL_STACK_ERROR;

    if (HDpipe(child_pfd) < 0)
        FAIL_STACK_ERROR;

    /* Fork child process */
    if ((childpid = HDfork()) < 0)
        FAIL_STACK_ERROR;

    /*
     * Child process as reader
     */
    if (childpid == 0) {
        int                    child_notify  = 0;               /* Notification between child and parent */
        hid_t                  fid_reader    = H5I_INVALID_HID; /* File ID for reader */
        hid_t                  fid_reader2   = H5I_INVALID_HID; /* File ID for reader */
        hid_t                  fid_reader3   = H5I_INVALID_HID; /* File ID for reader */
        hid_t                  fapl_reader   = H5I_INVALID_HID; /* File access property list for reader */
        H5F_vfd_swmr_config_t *config_reader = NULL;            /* VFD SWMR configuration */
        H5F_t *                file_reader;                     /* File pointer */
        eot_queue_entry_t *    curr;                            /* Pointer to an entry on the EOT queue */
        unsigned               count = 0;                       /* Counter */

        /* Close unused write end for writer pipe */
        if (HDclose(parent_pfd[1]) < 0)
            HDexit(EXIT_FAILURE);

        /* Close unused read end for reader pipe */
        if (HDclose(child_pfd[0]) < 0)
            HDexit(EXIT_FAILURE);

        /* Free unused configuration */
        if (config_writer)
            HDfree(config_writer);

        /*
         *  Open the file 3 times as VFD SWMR reader
         *  Enable and disable EOT for a file
         *  Verify the state of the EOT queue
         */

        /* Wait for notification 1 from parent to start verification */
        while (child_notify != 1) {
            if (HDread(parent_pfd[0], &child_notify, sizeof(int)) < 0)
                HDexit(EXIT_FAILURE);
        }

        /* Allocate memory for the configuration structure */
        if ((config_reader = HDmalloc(sizeof(*config_reader))) == NULL)
            HDexit(EXIT_FAILURE);

        /*
         * Set up the VFD SWMR configuration as reader + page buffering
         */

        /* config, tick_len, max_lag, writer, maintain_metadata_file, generate_updater_files,
           flush_raw_data, md_pages_reserved, md_file_path, updater_file_path */
        init_vfd_swmr_config(config_reader, 1, 3, FALSE, TRUE, FALSE, TRUE, 256, MD_FILENAME, NULL);

        /* use_latest_format, use_vfd_swmr, only_meta_page, page_buf_size, config */
        fapl_reader = vfd_swmr_create_fapl(FALSE, TRUE, FALSE, FS_PAGE_SIZE, config_reader);

        if (fapl_reader == H5I_INVALID_HID)
            FAIL_STACK_ERROR;

        /* Open the test file as reader */
        if ((fid_reader = H5Fopen(FILENAME, H5F_ACC_RDONLY, fapl_reader)) < 0)
            HDexit(EXIT_FAILURE);

        /* Open the same test file as reader (a second time) */
        if ((fid_reader2 = H5Fopen(FILENAME, H5F_ACC_RDONLY, fapl_reader)) < 0)
            HDexit(EXIT_FAILURE);

        /* Open the same test file as reader (a third time) */
        if ((fid_reader3 = H5Fopen(FILENAME, H5F_ACC_RDONLY, fapl_reader)) < 0)
            HDexit(EXIT_FAILURE);

        /* Verify the # of files on the EOT queue is 3 */
        count = 0;
        TAILQ_FOREACH(curr, &eot_queue_g, link)
        count++;
        if (count != 3)
            HDexit(EXIT_FAILURE);

        /* Disable EOT for the second opened file */
        if (H5Fvfd_swmr_disable_end_of_tick(fid_reader2) < 0)
            HDexit(EXIT_FAILURE);

        /* Verify the # of files on the EOT queue is 2 */
        count = 0;
        TAILQ_FOREACH(curr, &eot_queue_g, link)
        count++;
        if (count != 2)
            HDexit(EXIT_FAILURE);

        /* Get file pointer */
        file_reader = H5VL_object(fid_reader2);

        /* Should not find the second opened file on the EOT queue */
        TAILQ_FOREACH(curr, &eot_queue_g, link)
        {
            if (curr->vfd_swmr_file == file_reader)
                break;
        }
        if (curr != NULL && curr->vfd_swmr_file == file_reader)
            HDexit(EXIT_FAILURE);

        /* Enable EOT for the second opened file again */
        if (H5Fvfd_swmr_enable_end_of_tick(fid_reader2) < 0)
            HDexit(EXIT_FAILURE);

        /* Verify the # of files on the EOT queue is 3 */
        count = 0;
        TAILQ_FOREACH(curr, &eot_queue_g, link)
        count++;
        if (count != 3)
            HDexit(EXIT_FAILURE);

        /* Should find the second opened file on the EOT queue */
        TAILQ_FOREACH(curr, &eot_queue_g, link)
        {
            if (curr->vfd_swmr_file == file_reader)
                break;
        }
        if (curr == NULL || curr->vfd_swmr_file != file_reader)
            HDexit(EXIT_FAILURE);

        /* Closing */
        if (H5Fclose(fid_reader) < 0)
            HDexit(EXIT_FAILURE);
        if (H5Fclose(fid_reader2) < 0)
            HDexit(EXIT_FAILURE);
        if (H5Fclose(fid_reader3) < 0)
            HDexit(EXIT_FAILURE);
        if (H5Pclose(fapl_reader) < 0)
            HDexit(EXIT_FAILURE);
        if (config_reader)
            HDfree(config_reader);

        /* Close the pipes */
        if (HDclose(parent_pfd[0]) < 0)
            HDexit(EXIT_FAILURE);
        if (HDclose(child_pfd[1]) < 0)
            HDexit(EXIT_FAILURE);

        HDexit(EXIT_SUCCESS);
    } /* end child process */

    /*
     * Parent process as writer
     */

    /* Close unused read end for writer pipe */
    if (HDclose(parent_pfd[0]) < 0)
        FAIL_STACK_ERROR;

    /* Close unused write end for reader pipe */
    if (HDclose(child_pfd[1]) < 0)
        FAIL_STACK_ERROR;

    /*
     * Open the file as VFD SWMR writer
     */

    /* Open as VFD SWMR writer */
    if ((fid_writer = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl_writer)) < 0)
        FAIL_STACK_ERROR;

    /* Send notification 1 to reader to start verfication */
    notify = 1;
    if (HDwrite(parent_pfd[1], &notify, sizeof(int)) < 0)
        FAIL_STACK_ERROR;

    /*
     * Done
     */

    /* Close the pipes */
    if (HDclose(parent_pfd[1]) < 0)
        FAIL_STACK_ERROR;
    if (HDclose(child_pfd[0]) < 0)
        FAIL_STACK_ERROR;

    /* Wait for child process to complete */
    if ((tmppid = HDwaitpid(childpid, &child_status, child_wait_option)) < 0)
        FAIL_STACK_ERROR;

    /* Check exit status of child process */
    if (WIFEXITED(child_status)) {
        if ((child_exit_val = WEXITSTATUS(child_status)) != 0)
            TEST_ERROR;
    }
    else { /* child process terminated abnormally */
        TEST_ERROR;
    }

    /* Closing */
    if (H5Fclose(fid_writer) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(fapl_writer) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(fcpl) < 0)
        FAIL_STACK_ERROR;

    /* Free resources */
    if (config_writer)
        HDfree(config_writer);

    PASSED();
    return 0;

error:
    if (config_writer)
        HDfree(config_writer);

    H5E_BEGIN_TRY
    {
        H5Pclose(fapl_writer);
        H5Fclose(fid_writer);
        H5Pclose(fcpl);
    }
    H5E_END_TRY;

    return 1;
} /* test_disable_enable_eot_concur() */

/*-------------------------------------------------------------------------
 * Function:    test_file_end_tick_concur()
 *
 * Purpose:     Verify the public routine H5Fvfd_swmr_end_tick()
 *              triggers end of tick processing when the files
 *              are opened concurrently.
 *
 * Return:      0 if test is sucessful
 *              1 if test fails
 *
 * Programmer:  Vailin Choi; June 2020
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_file_end_tick_concur(void)
{
    hid_t                  fcpl          = -1;    /* File creation property list */
    hid_t                  fid_writer    = -1;    /* File ID for writer */
    hid_t                  fapl_writer   = -1;    /* File access property list for writer */
    H5F_vfd_swmr_config_t *config_writer = NULL;  /* VFD SWMR Configuration for writer */
    pid_t                  tmppid;                /* Child process ID returned by waitpid */
    pid_t                  childpid = 0;          /* Child process ID */
    int                    child_status;          /* Status passed to waitpid */
    int                    child_wait_option = 0; /* Options passed to waitpid */
    int                    child_exit_val;        /* Exit status of the child */

    int parent_pfd[2]; /* Pipe for parent process as writer */
    int child_pfd[2];  /* Pipe for child process as reader */
    int notify = 0;    /* Notification between parent and child */

    TESTING("Verify concurrent H5Fvfd_swmr_end_tick()");

    /* Allocate memory for the configuration structure */
    if ((config_writer = HDmalloc(sizeof(*config_writer))) == NULL)
        FAIL_STACK_ERROR;

    /*
     * Set up the VFD SWMR configuration + page buffering
     */

    /* config, tick_len, max_lag, writer, maintain_metadata_file, generate_updater_files,
    flush_raw_data, md_pages_reserved, md_file_path, updater_file_path */
    init_vfd_swmr_config(config_writer, 1, 3, TRUE, TRUE, FALSE, TRUE, 256, MD_FILENAME, NULL);

    /* use_latest_format, use_vfd_swmr, only_meta_page, page_buf_size, config */
    fapl_writer = vfd_swmr_create_fapl(FALSE, TRUE, FALSE, FS_PAGE_SIZE, config_writer);

    if (fapl_writer == H5I_INVALID_HID)
        FAIL_STACK_ERROR;

    if ((fcpl = vfd_swmr_create_fcpl(H5F_FSPACE_STRATEGY_PAGE, FS_PAGE_SIZE)) < 0) {
        HDprintf("vfd_swmr_create_fcpl() failed");
        FAIL_STACK_ERROR;
    }

    /* Create an HDF5 file with VFD SWMR configured */
    if ((fid_writer = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, fapl_writer)) < 0)
        FAIL_STACK_ERROR;

    /* Close the file */
    if (H5Fclose(fid_writer) < 0)
        FAIL_STACK_ERROR;

    /* Create 2 pipes */
    if (HDpipe(parent_pfd) < 0)
        FAIL_STACK_ERROR;

    if (HDpipe(child_pfd) < 0)
        FAIL_STACK_ERROR;

    /* Fork child process */
    if ((childpid = HDfork()) < 0)
        FAIL_STACK_ERROR;

    /*
     * Child process as reader
     */
    if (childpid == 0) {
        int                    child_notify  = 0;               /* Notification between child and parent */
        hid_t                  fid_reader1   = H5I_INVALID_HID; /* File ID for reader */
        hid_t                  fid_reader2   = H5I_INVALID_HID; /* File ID for reader */
        hid_t                  fid_reader3   = H5I_INVALID_HID; /* File ID for reader */
        hid_t                  fapl_reader   = H5I_INVALID_HID; /* File access property list for reader */
        H5F_vfd_swmr_config_t *config_reader = NULL;            /* VFD SWMR configuration */
        H5F_t *                f1, *f2, *f3;                    /* File pointer */
        uint64_t               s1 = 0;                          /* Saved tick_num */
        uint64_t               s2 = 0;                          /* Saved tick_num */
        uint64_t               s3 = 0;                          /* Saved tick_num */

        /* Close unused write end for writer pipe */
        if (HDclose(parent_pfd[1]) < 0)
            HDexit(EXIT_FAILURE);

        /* Close unused read end for reader pipe */
        if (HDclose(child_pfd[0]) < 0)
            HDexit(EXIT_FAILURE);

        /* Free unused configuration */
        if (config_writer)
            HDfree(config_writer);

        /*
         *  Open the file 3 times as VFD SWMR reader
         *  Trigger EOT for the files
         */

        /* Wait for notification 1 from parent to start verification */
        while (child_notify != 1) {
            if (HDread(parent_pfd[0], &child_notify, sizeof(int)) < 0)
                HDexit(EXIT_FAILURE);
        }

        /* Allocate memory for the configuration structure */
        if ((config_reader = HDmalloc(sizeof(*config_reader))) == NULL)
            HDexit(EXIT_FAILURE);

        /* config, tick_len, max_lag, writer, maintain_metadata_file, generate_updater_files,
           flush_raw_data, md_pages_reserved, md_file_path, updater_file_path */
        init_vfd_swmr_config(config_reader, 1, 3, FALSE, TRUE, FALSE, TRUE, 256, MD_FILENAME, NULL);

        /* use_latest_format, use_vfd_swmr, only_meta_page, page_buf_size, config */
        fapl_reader = vfd_swmr_create_fapl(FALSE, TRUE, FALSE, FS_PAGE_SIZE, config_reader);

        if (fapl_reader == H5I_INVALID_HID)
            FAIL_STACK_ERROR;

        /* Open the test file as reader */
        if ((fid_reader1 = H5Fopen(FILENAME, H5F_ACC_RDONLY, fapl_reader)) < 0)
            HDexit(EXIT_FAILURE);

        /* Open the same test file as reader (a second time) */
        if ((fid_reader2 = H5Fopen(FILENAME, H5F_ACC_RDONLY, fapl_reader)) < 0)
            HDexit(EXIT_FAILURE);

        /* Open the same test file as reader (a third time) */
        if ((fid_reader3 = H5Fopen(FILENAME, H5F_ACC_RDONLY, fapl_reader)) < 0)
            HDexit(EXIT_FAILURE);

        /* Get file pointer */
        f1 = H5VL_object(fid_reader1);
        f2 = H5VL_object(fid_reader2);
        f3 = H5VL_object(fid_reader3);

        /* Saved tick_num for the 3 files */
        s1 = f1->shared->tick_num;
        s2 = f2->shared->tick_num;
        s3 = f3->shared->tick_num;

        /* Trigger EOT for the second opened file */
        if (H5Fvfd_swmr_end_tick(fid_reader2) < 0)
            HDexit(EXIT_FAILURE);

        /* Verify tick_num should not be less than the previous tick_num */
        if (f2->shared->tick_num < s2)
            HDexit(EXIT_FAILURE);

        if (H5Fclose(fid_reader2) < 0)
            HDexit(EXIT_FAILURE);

        /* Trigger EOT for the first opened file */
        if (H5Fvfd_swmr_end_tick(fid_reader1) < 0)
            HDexit(EXIT_FAILURE);

        /* Verify tick_num should not be less than the previous tick_num */
        if (f1->shared->tick_num < s1)
            HDexit(EXIT_FAILURE);

        if (H5Fclose(fid_reader1) < 0)
            HDexit(EXIT_FAILURE);

        /* Trigger end tick processing for the third opened file */
        if (H5Fvfd_swmr_end_tick(fid_reader3) < 0)
            HDexit(EXIT_FAILURE);

        /* Verify tick_num should not be less than the previous tick_num */
        if (f3->shared->tick_num < s3)
            HDexit(EXIT_FAILURE);

        if (H5Fclose(fid_reader3) < 0)
            HDexit(EXIT_FAILURE);

        if (H5Pclose(fapl_reader) < 0)
            HDexit(EXIT_FAILURE);
        if (config_reader)
            HDfree(config_reader);

        /* Close the pipes */
        if (HDclose(parent_pfd[0]) < 0)
            HDexit(EXIT_FAILURE);
        if (HDclose(child_pfd[1]) < 0)
            HDexit(EXIT_FAILURE);

        HDexit(EXIT_SUCCESS);
    } /* end child process */

    /*
     * Parent process as writer
     */

    /* Close unused read end for writer pipe */
    if (HDclose(parent_pfd[0]) < 0)
        FAIL_STACK_ERROR;

    /* Close unused write end for reader pipe */
    if (HDclose(child_pfd[1]) < 0)
        FAIL_STACK_ERROR;

    /*
     * Open the file as VFD SWMR writer
     */

    /* Open as VFD SWMR writer */
    if ((fid_writer = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl_writer)) < 0)
        FAIL_STACK_ERROR;

    /* Send notification 1 to reader to start verfication */
    notify = 1;
    if (HDwrite(parent_pfd[1], &notify, sizeof(int)) < 0)
        FAIL_STACK_ERROR;

    /*
     * Done
     */

    /* Close the pipes */
    if (HDclose(parent_pfd[1]) < 0)
        FAIL_STACK_ERROR;
    if (HDclose(child_pfd[0]) < 0)
        FAIL_STACK_ERROR;

    /* Wait for child process to complete */
    if ((tmppid = HDwaitpid(childpid, &child_status, child_wait_option)) < 0)
        FAIL_STACK_ERROR;

    /* Check exit status of child process */
    if (WIFEXITED(child_status)) {
        if ((child_exit_val = WEXITSTATUS(child_status)) != 0)
            TEST_ERROR;
    }
    else { /* child process terminated abnormally */
        TEST_ERROR;
    }

    /* Closing */
    if (H5Fclose(fid_writer) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(fapl_writer) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(fcpl) < 0)
        FAIL_STACK_ERROR;

    /* Free resources */
    if (config_writer)
        HDfree(config_writer);

    PASSED();
    return 0;

error:
    if (config_writer)
        HDfree(config_writer);

    H5E_BEGIN_TRY
    {
        H5Pclose(fapl_writer);
        H5Fclose(fid_writer);
        H5Pclose(fcpl);
    }
    H5E_END_TRY;

    return 1;
} /* test_file_end_tick_concur() */

#endif /* H5_HAVE_UNISTD_H */

/*-------------------------------------------------------------------------
 * Function:    test_multiple_file_opens()
 *
 * Purpose:     Verify the entries on the EOT queue when opening files
 *              with and without VFD SWMR configured.
 *
 * Return:      0 if test is sucessful
 *              1 if test fails
 *
 * Programmer:  Vailin Choi; 11/18/2019
 *
 *-------------------------------------------------------------------------
 */

static unsigned
test_multiple_file_opens(void)
{
    hid_t                  fid1  = -1;     /* File ID */
    hid_t                  fid2  = -1;     /* File ID */
    hid_t                  fid   = -1;     /* File ID */
    hid_t                  fcpl  = -1;     /* File creation property list ID */
    hid_t                  fapl1 = -1;     /* File access property list ID */
    hid_t                  fapl2 = -1;     /* File access property list ID */
    H5F_t *                f1, *f2, *f;    /* File pointer */
    H5F_vfd_swmr_config_t *config1 = NULL; /* Configuration for VFD SWMR */
    H5F_vfd_swmr_config_t *config2 = NULL; /* Configuration for VFD SWMR */
    eot_queue_entry_t *    curr;

    TESTING("EOT queue entries when opening files with/without VFD SWMR");

    /* Allocate memory for the configuration structure */
    if ((config1 = HDmalloc(sizeof(*config1))) == NULL)
        FAIL_STACK_ERROR;
    if ((config2 = HDmalloc(sizeof(*config2))) == NULL)
        FAIL_STACK_ERROR;

    /*
     * Configured as VFD SWMR writer + page buffering
     */

    /* config, tick_len, max_lag, writer, maintain_metadata_file, generate_updater_files,
    flush_raw_data, md_pages_reserved, md_file_path, updater_file_path */
    init_vfd_swmr_config(config1, 4, 6, TRUE, TRUE, FALSE, TRUE, 2, MD_FILENAME, NULL);

    /* use_latest_format, use_vfd_swmr, only_meta_page, page_buf_size, config */
    fapl1 = vfd_swmr_create_fapl(FALSE, TRUE, FALSE, 4096, config1);
    if (fapl1 == H5I_INVALID_HID)
        FAIL_STACK_ERROR;

    /* config, tick_len, max_lag, writer, maintain_metadata_file, generate_updater_files,
    flush_raw_data, md_pages_reserved, md_file_path, updater_file_path */
    init_vfd_swmr_config(config2, 4, 6, TRUE, TRUE, FALSE, TRUE, 2, MD_FILENAME2, NULL);

    /* use_latest_format, use_vfd_swmr, only_meta_page, page_buf_size, config */
    fapl2 = vfd_swmr_create_fapl(FALSE, TRUE, FALSE, 4096, config2);

    if (fapl2 == H5I_INVALID_HID)
        FAIL_STACK_ERROR;

    if ((fcpl = vfd_swmr_create_fcpl(H5F_FSPACE_STRATEGY_PAGE, 4096)) < 0) {
        HDprintf("vfd_swmr_create_fcpl() failed");
        FAIL_STACK_ERROR;
    }

    /* Create a file without VFD SWMR */
    if ((fid = H5Fcreate(FNAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Get a pointer to the internal file object */
    if (NULL == (f = H5VL_object(fid)))
        FAIL_STACK_ERROR;

    /* Verify the global vfd_swmr_writer_g is not set */
    if ((curr = TAILQ_FIRST(&eot_queue_g)) != NULL && curr->vfd_swmr_writer)
        TEST_ERROR;
    /* The EOT queue should be empty */
    if (!TAILQ_EMPTY(&eot_queue_g))
        TEST_ERROR;

    /* Create a file with VFD SWMR writer */
    if ((fid1 = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, fapl1)) < 0)
        TEST_ERROR;

    /* Get a pointer to the internal file object */
    if (NULL == (f1 = H5VL_object(fid1)))
        FAIL_STACK_ERROR;

    /* Head of EOT queue should be a writer */
    if ((curr = TAILQ_FIRST(&eot_queue_g)) == NULL || !curr->vfd_swmr_writer)
        TEST_ERROR;
    /* The EOT queue should be initialized with the first entry equals to f1 */
    if ((curr = TAILQ_FIRST(&eot_queue_g)) == NULL || curr->vfd_swmr_file != f1)
        TEST_ERROR;

    /* Create another file with VFD SWMR writer */
    if ((fid2 = H5Fcreate(FILENAME2, H5F_ACC_TRUNC, fcpl, fapl2)) < 0)
        TEST_ERROR;

    /* Get a pointer to the internal file object */
    if (NULL == (f2 = H5VL_object(fid2)))
        FAIL_STACK_ERROR;

    /* Head of EOT queue should be a writer */
    if ((curr = TAILQ_FIRST(&eot_queue_g)) == NULL || !curr->vfd_swmr_writer)
        TEST_ERROR;
    /* The EOT queue's first entry should be f1 */
    if ((curr = TAILQ_FIRST(&eot_queue_g)) == NULL || curr->vfd_swmr_file != f1)
        TEST_ERROR;

    /* The file without VFD SWMR should not exist on the EOT queue */
    TAILQ_FOREACH(curr, &eot_queue_g, link)
    {
        if (curr->vfd_swmr_file == f)
            TEST_ERROR;
    }

    /* Close the first file with VFD SWMR */
    if (H5Fclose(fid1) < 0)
        FAIL_STACK_ERROR;

    /* Head of EOT queue should be a writer */
    if ((curr = TAILQ_FIRST(&eot_queue_g)) == NULL || !curr->vfd_swmr_writer)
        TEST_ERROR;
    /* The EOT queue's first entry should be f2 */
    if ((curr = TAILQ_FIRST(&eot_queue_g)) == NULL || curr->vfd_swmr_file != f2)
        TEST_ERROR;

    /* Close the second file with VFD SWMR */
    if (H5Fclose(fid2) < 0)
        FAIL_STACK_ERROR;

    /* Head of EOT queue should not be a writer */
    if ((curr = TAILQ_FIRST(&eot_queue_g)) != NULL && curr->vfd_swmr_writer)
        TEST_ERROR;
    /* The EOT queue should be empty */
    if (!TAILQ_EMPTY(&eot_queue_g))
        TEST_ERROR;

    /* Closing */
    if (H5Pclose(fapl1) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(fapl2) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(fcpl) < 0)
        FAIL_STACK_ERROR;
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Free buffers */
    if (config1)
        HDfree(config1);
    if (config2)
        HDfree(config2);

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(fapl1);
        H5Pclose(fapl2);
        H5Pclose(fcpl);
        H5Fclose(fid);
        H5Fclose(fid1);
        H5Fclose(fid2);
    }
    H5E_END_TRY;
    if (config1)
        HDfree(config1);
    if (config2)
        HDfree(config2);

    return 1;
} /* test_multiple_file_opens() */

/*-------------------------------------------------------------------------
 *  Function:    test_same_file_opens()
 *
 *  Purpose:     Verify multiple opens of the same file as listed below:
 *
 *                #1st open#
 *  #2nd open#    VW  VR   W   R
 *              ------------------
 *          VW  |  s   f   f   f |
 *          VR  |  f   f   f   f |
 *           W  |  f   f   s   f |
 *           R  |  f   f   s   s |
 *              ------------------
 *
 *  Notations:
 *        W:  H5F_ACC_RDWR
 *        R:  H5F_ACC_RDONLY
 *        VW: VFD SWMR writer
 *        VR: VFD SWMR reader
 *
 *        f: the open fails
 *        s: the open succeeds
 *
 * Return:      0 if test is sucessful
 *              1 if test fails
 *
 * Programmer:  Vailin Choi; October 2019
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_same_file_opens(void)
{
    hid_t                  fid     = -1;   /* File ID */
    hid_t                  fid2    = -1;   /* File ID */
    hid_t                  fcpl    = -1;   /* File creation property list ID */
    hid_t                  fapl1   = -1;   /* File access property list ID */
    hid_t                  fapl2   = -1;   /* File access property list ID */
    H5F_vfd_swmr_config_t *config1 = NULL; /* Configuration for VFD SWMR */
    H5F_vfd_swmr_config_t *config2 = NULL; /* Configuration for VFD SWMR */

    TESTING("Multiple opens of the same file with VFD SWMR configuration");

    /* Should succeed without VFD SWMR configured */
    if ((fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Close the file  */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Allocate memory for the configuration structure */
    if ((config1 = HDmalloc(sizeof(*config1))) == NULL)
        FAIL_STACK_ERROR;
    if ((config2 = HDmalloc(sizeof(*config2))) == NULL)
        FAIL_STACK_ERROR;

    if ((fcpl = vfd_swmr_create_fcpl(H5F_FSPACE_STRATEGY_PAGE, 4096)) < 0) {
        HDprintf("vfd_swmr_create_fcpl() failed");
        FAIL_STACK_ERROR;
    }

    /*
     * Tests for first column
     */

    /* Create the test file */
    if ((fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /*
     * Set the VFD SWMR configuration in fapl1 + page buffering
     */

    /* config, tick_len, max_lag, writer, maintain_metadata_file, generate_updater_files,
    flush_raw_data, md_pages_reserved, md_file_path, updater_file_path */
    init_vfd_swmr_config(config1, 4, 10, TRUE, TRUE, FALSE, TRUE, 2, MD_FILENAME, NULL);

    /* use_latest_format, use_vfd_swmr, only_meta_page, page_buf_size, config */
    fapl1 = vfd_swmr_create_fapl(FALSE, TRUE, FALSE, 4096, config1);

    if (fapl1 == H5I_INVALID_HID)
        FAIL_STACK_ERROR;

    /* Open the file as VFD SWMR writer */
    /* Keep the file open */
    if ((fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl1)) < 0)
        TEST_ERROR;

    /* Open the same file again as VFD SWMR writer */
    /* Should succeed: 1st open--VFD SWMR writer, 2nd open--VFD SWMR writer */
    if ((fid2 = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl1)) < 0)
        TEST_ERROR;

    /* Close the second file open */
    if (H5Fclose(fid2) < 0)
        FAIL_STACK_ERROR;

    /*
     * Set the VFD SWMR configuration in fapl2 + page buffering
     */

    /* config, tick_len, max_lag, writer, maintain_metadata_file, generate_updater_files,
    flush_raw_data, md_pages_reserved, md_file_path, updater_file_path */
    init_vfd_swmr_config(config2, 3, 8, FALSE, TRUE, FALSE, TRUE, 3, MD_FILENAME, NULL);

    /* use_latest_format, use_vfd_swmr, only_meta_page, page_buf_size, config */
    fapl2 = vfd_swmr_create_fapl(FALSE, TRUE, FALSE, 4096, config2);

    if (fapl2 == H5I_INVALID_HID)
        FAIL_STACK_ERROR;

    /* Open the same file again as VFD SWMR reader */
    /* Should fail: 1st open--VFD SWMR writer, 2nd open--VFD SWMR reader */
    H5E_BEGIN_TRY
    {
        fid2 = H5Fopen(FILENAME, H5F_ACC_RDONLY, fapl2);
    }
    H5E_END_TRY;
    if (fid2 >= 0)
        TEST_ERROR;

    if (H5Pclose(fapl2) < 0)
        FAIL_STACK_ERROR;

    /* Open the same file again as regular writer */
    /* Should fail: 1st open--VFD SWMR writer, 2nd open--regular writer */
    H5E_BEGIN_TRY
    {
        fid2 = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT);
    }
    H5E_END_TRY;
    if (fid2 >= 0)
        TEST_ERROR;

    /* Open the same file again as regular reader */
    /* Should fail: 1st open--VFD SWMR writer, 2nd open--regular reader */
    H5E_BEGIN_TRY
    {
        fid2 = H5Fopen(FILENAME, H5F_ACC_RDONLY, H5P_DEFAULT);
    }
    H5E_END_TRY;
    if (fid2 >= 0)
        TEST_ERROR;

    /* Close the 1st open file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /*
     * Tests for second column
     */

    /*
     * Set up as VFD SWMR reader + page buffering
     */

    /* config, tick_len, max_lag, writer, maintain_metadata_file, generate_updater_files,
    flush_raw_data, md_pages_reserved, md_file_path, updater_file_path */
    init_vfd_swmr_config(config1, 4, 10, FALSE, TRUE, FALSE, TRUE, 2, MD_FILENAME, NULL);

    /* use_latest_format, use_vfd_swmr, only_meta_page, page_buf_size, config */
    fapl1 = vfd_swmr_create_fapl(FALSE, TRUE, FALSE, 4096, config1);

    if (fapl1 == H5I_INVALID_HID)
        FAIL_STACK_ERROR;

    /* Open the file as VFD SWMR reader */
    /* Should fail because there is no metadata file */
    /* Take a while to complete due to retries */
    H5E_BEGIN_TRY
    {
        fid = H5Fopen(FILENAME, H5F_ACC_RDONLY, fapl1);
    }
    H5E_END_TRY;
    if (fid >= 0)
        TEST_ERROR;

    if (H5Pclose(fapl1) < 0)
        FAIL_STACK_ERROR;

    /*
     * Tests for third column
     */

    /* Open the file as regular writer */
    /* Keep the file open */
    if ((fid = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /*
     * Set up as VFD SWMR writer + page buffering
     */

    /* config, tick_len, max_lag, writer, maintain_metadata_file, generate_updater_files,
    flush_raw_data, md_pages_reserved, md_file_path, updater_file_path */
    init_vfd_swmr_config(config1, 4, 10, TRUE, TRUE, FALSE, TRUE, 2, MD_FILENAME, NULL);

    /* use_latest_format, use_vfd_swmr, only_meta_page, page_buf_size, config */
    fapl1 = vfd_swmr_create_fapl(FALSE, TRUE, FALSE, 4096, config1);

    if (fapl1 == H5I_INVALID_HID)
        FAIL_STACK_ERROR;

    /* Open the same file again as VFD SWMR writer */
    /* Should fail: 1st open--regular writer, 2nd open--VFD SWMR writer */
    H5E_BEGIN_TRY
    {
        fid2 = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl1);
    }
    H5E_END_TRY;
    if (fid2 >= 0)
        TEST_ERROR;

    if (H5Pclose(fapl1) < 0)
        FAIL_STACK_ERROR;

    /* Open the same file again as regular writer */
    /* Should succeed: 1st open--regular writer, 2nd open--regular writer */
    if ((fid2 = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if (H5Fclose(fid2) < 0)
        FAIL_STACK_ERROR;

    /* Open the same file again as regular reader */
    /* Should succeed: 1st open is writer, 2nd open is the same file as reader */
    if ((fid2 = H5Fopen(FILENAME, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if (H5Fclose(fid2) < 0)
        FAIL_STACK_ERROR;

    /* Close the 1st open file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /*
     * Tests for fourth column
     */

    /* Open the file as regular reader */
    /* keep the file open */
    if ((fid = H5Fopen(FILENAME, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /*
     * Set up as VFD SWMR writer + page buffering
     */

    /* config, tick_len, max_lag, writer, maintain_metadata_file, generate_updater_files,
    flush_raw_data, md_pages_reserved, md_file_path, updater_file_path */
    init_vfd_swmr_config(config1, 4, 10, TRUE, TRUE, FALSE, TRUE, 2, MD_FILENAME, NULL);

    /* use_latest_format, use_vfd_swmr, only_meta_page, page_buf_size, config */
    fapl1 = vfd_swmr_create_fapl(FALSE, TRUE, FALSE, 4096, config1);

    if (fapl1 == H5I_INVALID_HID)
        FAIL_STACK_ERROR;

    /* Open the same file again as VFD SMWR writer */
    /* Should fail: 1st open--regular reader, 2nd open--VFD SWMR writer */
    H5E_BEGIN_TRY
    {
        fid2 = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl1);
    }
    H5E_END_TRY;
    if (fid2 >= 0)
        TEST_ERROR;

    if (H5Pclose(fapl1) < 0)
        FAIL_STACK_ERROR;

    /* Open the same file again as regular reader */
    /* Should succeed: 1st open--regular reader, 2nd open--regular reader */
    if ((fid2 = H5Fopen(FILENAME, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if (H5Fclose(fid2) < 0)
        FAIL_STACK_ERROR;

    /* Open the same file again as regular writer */
    /* Should fail: 1st open--regular reader, 2nd open--regular writer */
    H5E_BEGIN_TRY
    {
        fid2 = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT);
    }
    H5E_END_TRY;
    if (fid2 >= 0)
        TEST_ERROR;

    /* Close the 1st open file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    if (H5Pclose(fcpl) < 0)
        FAIL_STACK_ERROR;

    /* Free buffers */
    if (config1)
        HDfree(config1);
    if (config2)
        HDfree(config2);

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(fapl1);
        H5Pclose(fapl2);
        H5Pclose(fcpl);
        H5Fclose(fid);
        H5Fclose(fid2);
    }
    H5E_END_TRY;
    if (config1)
        HDfree(config1);
    if (config2)
        HDfree(config2);
    return 1;
} /* test_same_file_opens() */

#ifndef _arraycount
#define _arraycount(_a) (sizeof(_a) / sizeof(_a[0]))
#endif

static unsigned
test_shadow_index_lookup(void)
{
    unsigned                   nerrors = 0;
    H5FD_vfd_swmr_idx_entry_t *idx;
    uint32_t                   size[] = {0, 1, 2, 3, 4, 0};
    unsigned                   seed   = 1;
    unsigned                   i, j, failj = UINT_MAX;
    hbool_t                    have_failj = FALSE;
    unsigned long              tmpl;
    const char *               seedvar = "H5_SHADOW_INDEX_SEED";
    const char *               failvar = "H5_SHADOW_INDEX_FAIL";

    TESTING("Shadow-index lookups");

    /* get seed from environment or else from time(3) */
    switch (fetch_env_ulong(seedvar, UINT_MAX, &tmpl)) {
        case -1:
            nerrors = 1;
            goto out;
        case 0:
            seed = (unsigned int)HDtime(NULL);
            break;
        default:
            seed = (unsigned int)tmpl;
            break;
    }

    /* get forced-fail index from environment */
    switch (fetch_env_ulong(failvar, UINT_MAX, &tmpl)) {
        case -1:
            nerrors = 1;
            goto out;
        case 0:
            break;
        default:
            failj      = (unsigned int)tmpl;
            have_failj = TRUE;
            break;
    }

    HDsrandom(seed);

    size[5] = (uint32_t)(1024 + HDrandom() % (16 * 1024 * 1024 - 1024));

    for (i = 0; i < _arraycount(size); i++) {
        uint32_t       cursize = size[i];
        const uint64_t modulus = UINT64_MAX / MAX(1, cursize);
        uint64_t       pageno;

        HDassert(modulus > 1); // so that modulus - 1 > 0, below

        idx = (cursize == 0) ? NULL : HDcalloc(cursize, sizeof(*idx));
        if (idx == NULL && cursize != 0) {
            HDfprintf(stderr, "couldn't allocate %" PRIu32 " indices\n", cursize);
            HDexit(EXIT_FAILURE);
        }
        for (pageno = (uint64_t)HDrandom() % modulus, j = 0; j < cursize;
             j++, pageno += 1 + (uint64_t)HDrandom() % (modulus - 1)) {
            idx[j].hdf5_page_offset = pageno;
        }
        for (j = 0; j < cursize; j++) {
            H5FD_vfd_swmr_idx_entry_t *found;

            found = vfd_swmr_pageno_to_mdf_idx_entry(idx, cursize, idx[j].hdf5_page_offset, FALSE);
            if ((have_failj && failj == j) || found != &idx[j])
                break;
        }
        if (j < cursize) {
            HDprintf("\nshadow-index entry %d lookup, pageno %" PRIu64 ", index size %" PRIu32 ", seed %u", j,
                     idx[j].hdf5_page_offset, cursize, seed);
            nerrors++;
        }
        if (idx != NULL)
            HDfree(idx);
    }

out:
    if (nerrors == 0)
        PASSED();
    else
        HDprintf(" FAILED\n");
    return nerrors;
}

/*-------------------------------------------------------------------------
 * Function:    test_enable_disable_eot()
 *
 * Purpose:     Verify the public routines:
 *                  H5Fvfd_swmr_enable_end_of_tick()
 *                  H5Fvfd_swmr_disable_end_of_tick()
 *               enables/disables EOT for the specified file
 *
 * Return:      0 if test is sucessful
 *              1 if test fails
 *
 * Programmer:  Vailin Choi; June 2020
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_enable_disable_eot(void)
{
    hid_t                  fid   = H5I_INVALID_HID; /* File ID */
    hid_t                  fid1  = H5I_INVALID_HID; /* File ID */
    hid_t                  fid2  = H5I_INVALID_HID; /* File ID */
    hid_t                  fid3  = H5I_INVALID_HID; /* File ID */
    hid_t                  fcpl  = H5I_INVALID_HID; /* File creation property list ID */
    hid_t                  fapl1 = H5I_INVALID_HID; /* File access property list ID */
    hid_t                  fapl2 = H5I_INVALID_HID; /* File access property list ID */
    hid_t                  fapl3 = H5I_INVALID_HID; /* File access property list ID */
    H5F_t *                f1, *f2, *f3;            /* File pointer */
    H5F_vfd_swmr_config_t *config1 = NULL;          /* Configuration for VFD SWMR */
    H5F_vfd_swmr_config_t *config2 = NULL;          /* Configuration for VFD SWMR */
    H5F_vfd_swmr_config_t *config3 = NULL;          /* Configuration for VFD SWMR */
    eot_queue_entry_t *    curr;                    /* Pointer to an entry on the EOT queue */
    unsigned               count = 0;               /* Counter */
    herr_t                 ret;                     /* Return value */

    TESTING("H5Fvfd_swmr_enable/disable_end_of_tick()");

    /* Allocate memory for the configuration structure */
    if ((config1 = HDmalloc(sizeof(*config1))) == NULL)
        FAIL_STACK_ERROR;
    if ((config2 = HDmalloc(sizeof(*config2))) == NULL)
        FAIL_STACK_ERROR;
    if ((config3 = HDmalloc(sizeof(*config3))) == NULL)
        FAIL_STACK_ERROR;

    /*
     * Configured first file as VFD SWMR writer + page buffering
     */

    /* config, tick_len, max_lag, writer, maintain_metadata_file, generate_updater_files,
    flush_raw_data, md_pages_reserved, md_file_path, updater_file_path */
    init_vfd_swmr_config(config1, 4, 6, TRUE, TRUE, FALSE, TRUE, 2, MD_FILENAME, NULL);

    /* use_latest_format, use_vfd_swmr, only_meta_page, page_buf_size, config */
    fapl1 = vfd_swmr_create_fapl(FALSE, TRUE, FALSE, 4096, config1);

    if (fapl1 == H5I_INVALID_HID)
        FAIL_STACK_ERROR;

    /*
     * Configured second file as VFD SWMR writer + page buffering
     */

    /* config, tick_len, max_lag, writer, maintain_metadata_file, generate_updater_files,
    flush_raw_data, md_pages_reserved, md_file_path, updater_file_path */
    init_vfd_swmr_config(config2, 4, 6, TRUE, TRUE, FALSE, TRUE, 2, MD_FILENAME2, NULL);

    /* use_latest_format, use_vfd_swmr, only_meta_page, page_buf_size, config */
    fapl2 = vfd_swmr_create_fapl(FALSE, TRUE, FALSE, 4096, config2);

    if (fapl2 == H5I_INVALID_HID)
        FAIL_STACK_ERROR;

    /*
     * Configured third file as VFD SWMR writer + page buffering
     */

    /* config, tick_len, max_lag, writer, maintain_metadata_file, generate_updater_files,
    flush_raw_data, md_pages_reserved, md_file_path, updater_file_path */
    init_vfd_swmr_config(config3, 4, 6, TRUE, TRUE, FALSE, TRUE, 2, MD_FILENAME3, NULL);

    /* use_latest_format, use_vfd_swmr, only_meta_page, page_buf_size, config */
    fapl3 = vfd_swmr_create_fapl(FALSE, TRUE, FALSE, 4096, config3);

    if (fapl3 == H5I_INVALID_HID)
        FAIL_STACK_ERROR;

    if ((fcpl = vfd_swmr_create_fcpl(H5F_FSPACE_STRATEGY_PAGE, 4096)) < 0) {
        HDprintf("vfd_swmr_create_fcpl() failed");
        FAIL_STACK_ERROR;
    }

    /* Create a file without VFD SWMR */
    if ((fid = H5Fcreate(FNAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Should fail to disable the file because VFD SWMR is not configured */
    H5E_BEGIN_TRY
    {
        ret = H5Fvfd_swmr_disable_end_of_tick(fid);
    }
    H5E_END_TRY;
    if (ret >= 0)
        TEST_ERROR;

    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    /* Create file 1 with VFD SWMR writer */
    if ((fid1 = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, fapl1)) < 0)
        TEST_ERROR;

    /* Create file 2 with VFD SWMR writer */
    if ((fid2 = H5Fcreate(FILENAME2, H5F_ACC_TRUNC, fcpl, fapl2)) < 0)
        TEST_ERROR;

    /* Create file 3 with VFD SWMR writer */
    if ((fid3 = H5Fcreate(FILENAME3, H5F_ACC_TRUNC, fcpl, fapl3)) < 0)
        TEST_ERROR;

    /* Should have 3 files on the EOT queue */
    TAILQ_FOREACH(curr, &eot_queue_g, link)
    count++;
    if (count != 3)
        TEST_ERROR;

    /* Disable EOT for file 1 */
    if (H5Fvfd_swmr_disable_end_of_tick(fid1) < 0)
        TEST_ERROR;

    /* Disable file 1 again should fail because the file has just been disabled */
    H5E_BEGIN_TRY
    {
        ret = H5Fvfd_swmr_disable_end_of_tick(fid1);
    }
    H5E_END_TRY;
    if (ret >= 0)
        TEST_ERROR;

    /* Should have 2 files on the EOT queue */
    count = 0;
    TAILQ_FOREACH(curr, &eot_queue_g, link)
    count++;
    if (count != 2)
        TEST_ERROR;

    /* Get a pointer to the internal file object */
    if (NULL == (f1 = H5VL_object(fid1)))
        FAIL_STACK_ERROR;

    /* Should not find file 1 on the EOT queue */
    TAILQ_FOREACH(curr, &eot_queue_g, link)
    {
        if (curr->vfd_swmr_file == f1)
            break;
    }
    if (curr != NULL && curr->vfd_swmr_file == f1)
        TEST_ERROR;

    /* Enable EOT for file 2 should fail because the file has not been disabled */
    H5E_BEGIN_TRY
    {
        ret = H5Fvfd_swmr_enable_end_of_tick(fid2);
    }
    H5E_END_TRY;
    if (ret >= 0)
        TEST_ERROR;

    /* Get a pointer to the internal file object */
    if (NULL == (f2 = H5VL_object(fid2)))
        FAIL_STACK_ERROR;

    /* File 2 should be on the EOT queue */
    TAILQ_FOREACH(curr, &eot_queue_g, link)
    {
        if (curr->vfd_swmr_file == f2)
            break;
    }
    if (curr == NULL || curr->vfd_swmr_file != f2)
        TEST_ERROR;

    /* Close file 3 */
    if (H5Fclose(fid3) < 0)
        TEST_ERROR;

    /* Open file 3 again without VFD SWMR writer */
    if ((fid3 = H5Fopen(FILENAME3, H5F_ACC_RDWR, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Get a pointer to the internal file object for file 3 */
    if (NULL == (f3 = H5VL_object(fid3)))
        FAIL_STACK_ERROR;

    /* File 3 should not exist on the EOT queue */
    TAILQ_FOREACH(curr, &eot_queue_g, link)
    {
        if (curr->vfd_swmr_file == f3)
            break;
    }
    if (curr != NULL && curr->vfd_swmr_file == f3)
        TEST_ERROR;

    /* Should have 2 files on the EOT queue */
    count = 0;
    TAILQ_FOREACH(curr, &eot_queue_g, link)
    count++;
    if (count != 1)
        TEST_ERROR;

    /* Should fail to enable file 3 */
    H5E_BEGIN_TRY
    {
        ret = H5Fvfd_swmr_enable_end_of_tick(fid3);
    }
    H5E_END_TRY;
    if (ret >= 0)
        TEST_ERROR;

    /* Should fail to disable file 3 */
    H5E_BEGIN_TRY
    {
        ret = H5Fvfd_swmr_disable_end_of_tick(fid3);
    }
    H5E_END_TRY;
    if (ret >= 0)
        TEST_ERROR;

    /* Closing */
    if (H5Fclose(fid1) < 0)
        FAIL_STACK_ERROR;
    if (H5Fclose(fid2) < 0)
        FAIL_STACK_ERROR;
    if (H5Fclose(fid3) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(fapl1) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(fapl2) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(fapl3) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(fcpl) < 0)
        FAIL_STACK_ERROR;

    /* Free buffers */
    if (config1)
        HDfree(config1);
    if (config2)
        HDfree(config2);
    if (config3)
        HDfree(config3);

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(fapl1);
        H5Pclose(fapl2);
        H5Pclose(fapl3);
        H5Pclose(fcpl);
        H5Fclose(fid);
        H5Fclose(fid1);
        H5Fclose(fid2);
        H5Fclose(fid3);
    }
    H5E_END_TRY;
    if (config1)
        HDfree(config1);
    if (config2)
        HDfree(config2);
    if (config3)
        HDfree(config3);

    return 1;
} /* test_enable_disable_eot() */

/*-------------------------------------------------------------------------
 * Function:    verify_updater_flags()
 *
 * Purpose:     This is the helper routine used to verify whether
 *              "flags" in the updater file is as expected.
 *
 * Return:      0 if test is sucessful
 *              1 if test fails
 *
 * Programmer:  Vailin Choi; October 2021
 *
 *-------------------------------------------------------------------------
 */
static herr_t
verify_updater_flags(char *ud_name, uint16_t expected_flags)
{
    FILE *   ud_fp = NULL; /* Updater file pointer */
    uint16_t flags = 0;    /* The "flags" field in the updater file */

    /* Open the updater file */
    if ((ud_fp = HDfopen(ud_name, "r")) == NULL)
        FAIL_STACK_ERROR;

    /* Seek to the position of "flags" in the updater file's header */
    if (HDfseek(ud_fp, (HDoff_t)UD_HD_FLAGS_OFFSET, SEEK_SET) < 0)
        FAIL_STACK_ERROR;

    /* Read "flags" from the updater file */
    if (HDfread(&flags, UD_SIZE_2, 1, ud_fp) != (size_t)1)
        FAIL_STACK_ERROR;

    if (flags != expected_flags)
        TEST_ERROR;

    if (HDfclose(ud_fp) < 0)
        FAIL_STACK_ERROR;

    return 0;

error:
    return 1;

} /* verify_updater_flags() */

/*-------------------------------------------------------------------------
 * Function:    test_updater_flags
 *
 * Purpose:     Verify "flags" in the updater file is as expected for
 *              file creation.
 *
 * Return:      0 if test is sucessful
 *              1 if test fails
 *
 * Programmer:  Vailin Choi; October 2021
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_updater_flags(void)
{
    hid_t                  fid         = -1;   /* File ID */
    hid_t                  fcpl        = -1;   /* File creation property list ID */
    hid_t                  fapl        = -1;   /* File access property list ID */
    hid_t                  file_fapl   = -1;   /* File access property list ID associated with the file */
    H5F_vfd_swmr_config_t *config      = NULL; /* Configuration for VFD SWMR */
    H5F_vfd_swmr_config_t *file_config = NULL; /* Configuration for VFD SWMR */
    uint64_t               seq_num     = 0;    /* Sequence number for updater file */
    uint64_t               i           = 0;    /* Local index variable */
    char                   namebuf[H5F__MAX_VFD_SWMR_FILE_NAME_LEN]; /* Updater file path */
    h5_stat_t              sb;                                       /* Info returned by stat system call */

    TESTING("VFD SWMR updater file flags");

    /* Should succeed without VFD SWMR configured */
    if ((fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Close the file  */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Allocate memory for the configuration structure */
    if ((config = (H5F_vfd_swmr_config_t *)HDmalloc(sizeof(H5F_vfd_swmr_config_t))) == NULL)
        FAIL_STACK_ERROR;
    if ((file_config = (H5F_vfd_swmr_config_t *)HDmalloc(sizeof(H5F_vfd_swmr_config_t))) == NULL)
        FAIL_STACK_ERROR;

    /*
     * Configured as VFD SWMR writer + page buffering + generate updater files
     */

    /* config, tick_len, max_lag, writer, maintain_metadata_file, generate_updater_files,
       flush_raw_data, md_pages_reserved, md_file_path, updater_file_path */
    init_vfd_swmr_config(config, 4, 7, TRUE, TRUE, TRUE, TRUE, 2, MD_FILENAME, UD_FILENAME);

    /* use_latest_format, use_vfd_swmr, only_meta_page, page_buf_size, config */
    fapl = vfd_swmr_create_fapl(FALSE, TRUE, FALSE, 4096, config);

    if (fapl == H5I_INVALID_HID)
        TEST_ERROR;

    if ((fcpl = vfd_swmr_create_fcpl(H5F_FSPACE_STRATEGY_PAGE, 4096)) < 0) {
        HDprintf("vfd_swmr_create_fcpl() failed");
        FAIL_STACK_ERROR;
    }

    if ((fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, fapl)) < 0)
        TEST_ERROR;

    /* Get the file's file access property list */
    if ((file_fapl = H5Fget_access_plist(fid)) < 0)
        FAIL_STACK_ERROR;

    /* Retrieve the VFD SWMR configuration from file_fapl */
    if (H5Pget_vfd_swmr_config(file_fapl, file_config) < 0)
        TEST_ERROR;

    /* Verify the retrieved info is the same as config1 */
    if (HDmemcmp(config, file_config, sizeof(H5F_vfd_swmr_config_t)) != 0)
        TEST_ERROR;

    /* Verify the first updater file: "flags" field and file size */
    HDsprintf(namebuf, "%s.%lu", UD_FILENAME, seq_num);

    /* Verify "flags" of the first updater file */
    if (verify_updater_flags(namebuf, CREATE_METADATA_FILE_ONLY_FLAG) < 0)
        TEST_ERROR;

    /* Check updater file size */
    if (HDstat(namebuf, &sb) == 0 && sb.st_size != H5F_UD_HEADER_SIZE)
        TEST_ERROR;

    /* Closing */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Look for the last updater file */
    for (seq_num = 0;; seq_num++) {
        HDsprintf(namebuf, "%s.%lu", UD_FILENAME, seq_num);
        if (HDaccess(namebuf, F_OK) != 0)
            break;
    }
    HDsprintf(namebuf, "%s.%lu", UD_FILENAME, seq_num - 1);

    /* Verify "flags" of the last updater file */
    if (verify_updater_flags(namebuf, FINAL_UPDATE_FLAG) < 0)
        TEST_ERROR;

    if (H5Pclose(file_fapl) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(fcpl) < 0)
        FAIL_STACK_ERROR;

    /* Remove updater files */
    for (i = 0; i < seq_num; i++) {
        HDsprintf(namebuf, "%s.%lu", UD_FILENAME, i);
        HDremove(namebuf);
    }

    /* Free buffers */
    if (config)
        HDfree(config);
    if (file_config)
        HDfree(file_config);

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(fapl);
        H5Pclose(file_fapl);
        H5Pclose(fcpl);
        H5Fclose(fid);
    }
    H5E_END_TRY;
    if (config)
        HDfree(config);
    if (file_config)
        HDfree(file_config);

    return 1;
} /* test_updater_flags() */

/*-------------------------------------------------------------------------
 * Function:    test_updater_flags_same_file_opens()
 *
 * Purpose:     Verify "flags" in the updater file is as expected for
 *              multiple opens of the same file.
 *
 * Return:      0 if test is sucessful
 *              1 if test fails
 *
 * Programmer:  Vailin Choi; October 2021
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_updater_flags_same_file_opens(void)
{
    hid_t                  fid     = -1;                             /* File ID */
    hid_t                  fid2    = -1;                             /* File ID */
    hid_t                  fcpl    = -1;                             /* File creation property list ID */
    hid_t                  fapl1   = -1;                             /* File access property list ID */
    H5F_vfd_swmr_config_t *config1 = NULL;                           /* Configuration for VFD SWMR */
    uint64_t               seq_num = 0;                              /* Sequence number for updater file */
    uint64_t               i       = 0;                              /* Local index variable */
    char                   namebuf[H5F__MAX_VFD_SWMR_FILE_NAME_LEN]; /* Updater file path */

    TESTING("VFD SWMR updater file flags for multiple opens of the same file");

    /* Should succeed without VFD SWMR configured */
    if ((fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Close the file  */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Allocate memory for the configuration structure */
    if ((config1 = HDmalloc(sizeof(*config1))) == NULL)
        FAIL_STACK_ERROR;

    if ((fcpl = vfd_swmr_create_fcpl(H5F_FSPACE_STRATEGY_PAGE, 4096)) < 0) {
        HDprintf("vfd_swmr_create_fcpl() failed");
        FAIL_STACK_ERROR;
    }

    /* Create the test file */
    if ((fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /*
     * Set the VFD SWMR configuration in fapl1 + page buffering
     */

    /* config, tick_len, max_lag, writer, maintain_metadata_file, generate_updater_files,
    flush_raw_data, md_pages_reserved, md_file_path, updater_file_path */
    init_vfd_swmr_config(config1, 4, 10, TRUE, TRUE, TRUE, TRUE, 2, MD_FILENAME, UD_FILENAME);

    /* use_latest_format, use_vfd_swmr, only_meta_page, page_buf_size, config */
    fapl1 = vfd_swmr_create_fapl(FALSE, TRUE, FALSE, 4096, config1);

    if (fapl1 == H5I_INVALID_HID)
        FAIL_STACK_ERROR;

    /* Open the file as VFD SWMR writer */
    /* Keep the file open */
    if ((fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl1)) < 0)
        TEST_ERROR;

    /* Open the same file again as VFD SWMR writer */
    /* Should succeed: 1st open--VFD SWMR writer, 2nd open--VFD SWMR writer */
    if ((fid2 = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl1)) < 0)
        TEST_ERROR;

    /* Close the second file open */
    if (H5Fclose(fid2) < 0)
        FAIL_STACK_ERROR;

    /* Verify the first updater file for first file open */
    HDsprintf(namebuf, "%s.%lu", UD_FILENAME, seq_num);

    /* Verify "flags" of the first updater file is 0*/
    if (verify_updater_flags(namebuf, 0) < 0)
        TEST_ERROR;

    /* Look for the last updater file */
    for (seq_num = 0;; seq_num++) {
        HDsprintf(namebuf, "%s.%lu", UD_FILENAME, seq_num);
        if (HDaccess(namebuf, F_OK) != 0)
            break;
    }
    HDsprintf(namebuf, "%s.%lu", UD_FILENAME, seq_num - 1);

    /* Verify "flags" of the last updater file is 0 */
    if (verify_updater_flags(namebuf, 0) < 0)
        TEST_ERROR;

    /* Close the 1st open file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Look for the last updater file */
    for (seq_num = 0;; seq_num++) {
        HDsprintf(namebuf, "%s.%lu", UD_FILENAME, seq_num);
        if (HDaccess(namebuf, F_OK) != 0)
            break;
    }
    HDsprintf(namebuf, "%s.%lu", UD_FILENAME, seq_num - 1);

    /* Verify "flags" of the last updater file after closing file */
    if (verify_updater_flags(namebuf, FINAL_UPDATE_FLAG) < 0)
        TEST_ERROR;

    /* Clean up updater files */
    for (i = 0; i < seq_num; i++) {
        HDsprintf(namebuf, "%s.%lu", UD_FILENAME, i);
        HDremove(namebuf);
    }

    if (H5Pclose(fcpl) < 0)
        FAIL_STACK_ERROR;

    /* Free buffers */
    if (config1)
        HDfree(config1);

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(fapl1);
        H5Pclose(fcpl);
        H5Fclose(fid);
        H5Fclose(fid2);
    }
    H5E_END_TRY;
    if (config1)
        HDfree(config1);
    return 1;
} /* test_updater_flags_same_file_opens() */

/*-------------------------------------------------------------------------
 * Function:    clean_chk_ud_files()
 *
 * Purpose:     This is the helper routine used to clean up
 *              the checksum file and the updater files.
 *
 * Return:      void
 *
 * Programmer:  Vailin Choi; October 2021
 *
 *-------------------------------------------------------------------------
 */
static void
clean_chk_ud_files(char *md_file_path, char *updater_file_path)
{
    char     chk_name[FILE_NAME_LEN]; /* Checksum file name */
    char     ud_name[FILE_NAME_LEN];  /* Updater file name */
    uint64_t i;

    /* Name of the checksum file: <md_file_path>.chk */
    HDsprintf(chk_name, "%s.chk", md_file_path);

    /* Remove the checksum file if exists.
       If not, the callback will just continue appending
       checksums to the existing file */
    if (HDaccess(chk_name, F_OK) == 0) {
        HDremove(chk_name);
    }

    /* Remove all the updater files if exist: <updater_file_path>.<i> */
    for (i = 0;; i++) {
        HDsprintf(ud_name, "%s.%lu", updater_file_path, i);
        if (HDaccess(ud_name, F_OK) != 0)
            break;
        HDremove(ud_name);
    }

} /* clean_chk_ud_files() */

/*-------------------------------------------------------------------------
 * Function:    verify_ud_chk()
 *
 * Purpose:     This is the helper routine used by
 *              test_updater_generate_md_checksums() to verify
 *              contents of the checksum file and the updater files.
 *              --verify the sequence number in each updater's file header
 *                corresponds to the ith sequence number of the updater
 *                file name.
 *              --verify the tick number in each updater's file header
 *                corresponds to the tick number stored in the checksum file
 *              --verify the change_list_len in each updater's file header
 *                is consistent with num_change_list_entries in each updater's
 *                change list header
 *
 * Return:      0 if test is sucessful
 *              1 if test fails
 *
 * Programmer:  Vailin Choi; October 2021
 *
 *-------------------------------------------------------------------------
 */
static herr_t
verify_ud_chk(char *md_file_path, char *ud_file_path)
{
    char     chk_name[FILE_NAME_LEN];     /* Checksum file name */
    char     ud_name[FILE_NAME_LEN];      /* Updater file name */
    FILE *   chk_fp         = NULL;       /* Checksum file pointer */
    FILE *   ud_fp          = NULL;       /* Updater file pointer */
    uint64_t ud_seq_num     = 0;          /* Sequence number in the updater file */
    uint64_t chk_ud_seq_num = 0;          /* Updater sequence number in the checksum file */
    uint64_t i;                           /* Local index variable */
    long     size                    = 0; /* Size of the file */
    size_t   change_list_len         = 0; /* change_list_len in the updater file header */
    uint32_t num_change_list_entries = 0; /* num_change_list_entries in the updater change list header */

    /* Open the checksum file */
    HDsprintf(chk_name, "%s.chk", md_file_path);
    if ((chk_fp = HDfopen(chk_name, "r")) == NULL)
        FAIL_STACK_ERROR;

    for (i = 0;; i++) {
        /* Generate updater file name: <ud_file_path>.<i> */
        HDsprintf(ud_name, "%s.%lu", ud_file_path, i);

        /* Open the updater file */
        if ((ud_fp = HDfopen(ud_name, "r")) == NULL)
            break;
        else {
            /* Seek to the position of the sequence number in the updater file's header */
            if (HDfseek(ud_fp, (off_t)UD_HD_SEQ_NUM_OFFSET, SEEK_SET) < 0)
                FAIL_STACK_ERROR;

            /* Read the sequence number from the updater file */
            if (HDfread(&ud_seq_num, UD_SIZE_8, 1, ud_fp) != 1)
                FAIL_STACK_ERROR;

            /* Compare the sequence number with i */
            if (ud_seq_num != i)
                TEST_ERROR;

            /* Read change_list_len from updater file's header */
            if (HDfseek(ud_fp, (off_t)UD_HD_CHANGE_LIST_LEN_OFFSET, SEEK_SET) < 0)
                FAIL_STACK_ERROR;

            if (HDfread(&change_list_len, UD_SIZE_8, 1, ud_fp) != 1)
                FAIL_STACK_ERROR;

            if (i != 0) {

                /* Read num_change_list_entries from updater file's change list */
                if (HDfseek(ud_fp, (off_t)UD_CL_NUM_CHANGE_LIST_ENTRIES_OFFSET, SEEK_SET) < 0)
                    FAIL_STACK_ERROR;

                if (HDfread(&num_change_list_entries, UD_SIZE_4, 1, ud_fp) != 1)
                    FAIL_STACK_ERROR;

                if (num_change_list_entries == 0) {
                    if (change_list_len != H5F_UD_CL_SIZE(0))
                        TEST_ERROR;
                }
                else {
                    if (change_list_len != H5F_UD_CL_SIZE(num_change_list_entries))
                        TEST_ERROR;
                }
            }

            /* Close the updater file */
            if (HDfclose(ud_fp) < 0)
                FAIL_STACK_ERROR;

            /* Read the updater sequence number from checksum file */
            if (HDfread(&chk_ud_seq_num, UD_SIZE_8, 1, chk_fp) != 1)
                FAIL_STACK_ERROR;

            /* Compare sequence number in updater file with sequence number in checksum file */
            if (ud_seq_num != chk_ud_seq_num)
                TEST_ERROR;

            /* Advance checksum file to the next sequence number */
            if (HDfseek(chk_fp, (off_t)UD_SIZE_4, SEEK_CUR) < 0)
                FAIL_STACK_ERROR;
        }
    }

    /* Get the size of the chksum file */
    if ((size = HDftell(chk_fp)) < 0)
        FAIL_STACK_ERROR;

    /* Size of sequence number and checksum in the checksum file */
    if ((unsigned)size != (i * (UD_SIZE_8 + UD_SIZE_4)))
        TEST_ERROR;

    return 0;

error:
    return (-1);

} /* verify_ud_chk() */

/*-------------------------------------------------------------------------
 * Function:    md_ck_cb()
 *
 * Purpose:     This is the callback function used by
 *              test_updater_generate_md_checksums() when the
 *              H5F_ACS_GENERATE_MD_CK_CB_NAME property is set in fapl.
 *                  --Open and read the metadata file into a buffer.
 *                  --Generate checksum for the metadata file
 *                  --Write the tick number and the checksum to the checksum file
 *
 * Return:      0 if test is sucessful
 *              1 if test fails
 *
 * Programmer:  Vailin Choi; October 2021
 *
 *-------------------------------------------------------------------------
 */
static herr_t
md_ck_cb(char *md_file_path, uint64_t updater_seq_num)
{
    FILE *   md_fp  = NULL;           /* Metadata file pointer */
    FILE *   chk_fp = NULL;           /* Checksum file pointer */
    long     size   = 0;              /* File size returned from HDftell() */
    void *   buf    = NULL;           /* Buffer for holding the metadata file content */
    uint32_t chksum = 0;              /* The checksum generated for the metadata file */
    char     chk_name[FILE_NAME_LEN]; /* Buffer for the checksum file name */
    size_t   ret;                     /* Return value */

    /* Open the metadata file */
    if ((md_fp = HDfopen(md_file_path, "r")) == NULL)
        FAIL_STACK_ERROR;

    /* Set file pointer at end of file.*/
    if (HDfseek(md_fp, 0, SEEK_END) < 0)
        FAIL_STACK_ERROR;

    /* Get the current position of the file pointer.*/
    if ((size = HDftell(md_fp)) < 0)
        FAIL_STACK_ERROR;

    if (size != 0) {

        HDrewind(md_fp);

        if ((buf = HDmalloc((size_t)size)) == NULL)
            FAIL_STACK_ERROR;

        /* Read the metadata file to buf */
        if ((ret = HDfread(buf, 1, (size_t)size, md_fp)) != (size_t)size)
            FAIL_STACK_ERROR;

        /* Calculate checksum of the metadata file */
        chksum = H5_checksum_metadata(buf, (size_t)size, 0);
    }

    /* Close the metadata file */
    if (md_fp && HDfclose(md_fp) < 0)
        FAIL_STACK_ERROR;

    /*
     *  Checksum file
     */

    /* Generate checksum file name: <md_file_path>.chk */
    HDsprintf(chk_name, "%s.chk", md_file_path);

    /* Open checksum file for append */
    if ((chk_fp = HDfopen(chk_name, "a")) == NULL)
        FAIL_STACK_ERROR;

    /* Write the updater sequence number to the checksum file */
    if ((ret = HDfwrite(&updater_seq_num, sizeof(uint64_t), 1, chk_fp)) != 1)
        FAIL_STACK_ERROR;

    /* Write the checksum to the checksum file */
    if ((ret = HDfwrite(&chksum, sizeof(uint32_t), 1, chk_fp)) != 1)
        FAIL_STACK_ERROR;

    /* Close the checksum file */
    if (chk_fp && HDfclose(chk_fp) != 0)
        FAIL_STACK_ERROR;

    if (buf)
        HDfree(buf);

    return 0;

error:
    if (buf)
        HDfree(buf);
    if (md_fp)
        HDfclose(md_fp);
    if (chk_fp)
        HDfclose(chk_fp);

    return -1;
} /* md_ck_cb() */

/*-------------------------------------------------------------------------
 * Function:    test_updater_generate_md_checksums()
 *
 * Purpose:     It enables the generation of checksums for the metadata file
 *              created by the writer end of tick function.
 *              It also verifies the contents of the checksum file and the
 *              updater files.
 *
 *              The test is invoked when the file is created via H5Fcreate()
 *              and via H5Fopen().
 *
 * Return:      0 if test is sucessful
 *              1 if test fails
 *
 * Programmer:  Vailin Choi; October 2021
 *
 * Note: It is important to clean up the checksum file and the updater files.
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_updater_generate_md_checksums(hbool_t file_create)
{
    hid_t                   fid  = -1; /* File ID */
    hid_t                   fcpl = -1; /* File creation property list ID */
    hid_t                   fapl = -1; /* File access property list ID */
    H5F_vfd_swmr_config_t   config;    /* Configuration for VFD SWMR */
    H5F_generate_md_ck_cb_t cb_info;   /* Callback */

    if (file_create) {
        TESTING("VFD SWMR updater generate checksums for metadata file with H5Fcreate");
    }
    else {
        TESTING("VFD SWMR updater generate checksums for metadata file with H5Fopen");
    }

    /* config, tick_len, max_lag, writer, maintain_metadata_file, generate_updater_files,
     * flush_raw_data, md_pages_reserved, md_file_path, updater_file_path */
    init_vfd_swmr_config(&config, 4, 7, TRUE, TRUE, TRUE, TRUE, 2, MD_FILE, UD_FILE);

    /* use_latest_format, use_vfd_swmr, only_meta_page, page_buf_size, config */
    fapl = vfd_swmr_create_fapl(TRUE, TRUE, FALSE, 4096, &config);

    if (fapl == H5I_INVALID_HID)
        TEST_ERROR;

    if ((fcpl = vfd_swmr_create_fcpl(H5F_FSPACE_STRATEGY_PAGE, 4096)) < 0) {
        HDprintf("vfd_swmr_create_fcpl() failed");
        FAIL_STACK_ERROR;
    }

    /* Set up callback to generate checksums for updater's metadata files */
    cb_info.func = md_ck_cb;

    /* Activate private property to generate checksums for updater's metadata file */
    if (H5Pset(fapl, H5F_ACS_GENERATE_MD_CK_CB_NAME, &cb_info) < 0)
        FAIL_STACK_ERROR;

    /* Use file creation or file open for testing */
    if (file_create) {
        if ((fid = H5Fcreate(FILENAME4, H5F_ACC_TRUNC, fcpl, fapl)) < 0)
            FAIL_STACK_ERROR;
    }
    else {
        if ((fid = H5Fcreate(FILENAME4, H5F_ACC_TRUNC, fcpl, H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR;
        if (H5Fclose(fid) < 0)
            FAIL_STACK_ERROR;

        if ((fid = H5Fopen(FILENAME4, H5F_ACC_RDWR, fapl)) < 0)
            FAIL_STACK_ERROR;
    }

    /* Close the file  */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    if (H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(fcpl) < 0)
        FAIL_STACK_ERROR;

    /* Verify contents of checksum file and updater files */
    if (verify_ud_chk(config.md_file_path, config.updater_file_path) < 0)
        TEST_ERROR;

    /*  It's important to clean up the checksum and updater files. */
    clean_chk_ud_files(config.md_file_path, config.updater_file_path);

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(fapl);
        H5Pclose(fcpl);
        H5Fclose(fid);
    }
    H5E_END_TRY;

    /*  It's important to clean up the chechsum and updater files. */
    clean_chk_ud_files(config.md_file_path, config.updater_file_path);

    return 1;

} /* test_updater_generate_md_checksums() */

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
    hid_t fapl = -1;                 /* File access property list for */
                                     /* data files                    */
    unsigned    nerrors      = 0;    /* Cumulative error count */
    char *      lock_env_var = NULL; /* File locking env var pointer */
    const char *env_h5_drvr  = NULL; /* File Driver value from environment */
    hbool_t     use_file_locking;    /* Read from env var */

    /* Check the environment variable that determines if we care
     * about file locking. File locking should be used unless explicitly
     * disabled.
     */
    lock_env_var = HDgetenv("HDF5_USE_FILE_LOCKING");
    if (lock_env_var && !HDstrcmp(lock_env_var, "FALSE"))
        use_file_locking = FALSE;
    else
        use_file_locking = TRUE;

    /* Get the VFD to use */
    env_h5_drvr = HDgetenv("HDF5_DRIVER");
    if (env_h5_drvr == NULL)
        env_h5_drvr = "nomatch";

    /* Temporary skip testing with multi/split drivers:
     * Page buffering depends on paged aggregation which is
     * currently disabled for multi/split drivers.
     */
    if ((0 == HDstrcmp(env_h5_drvr, "multi")) || (0 == HDstrcmp(env_h5_drvr, "split"))) {
        HDputs("Skip VFD SWMR test because paged aggregation is disabled for multi/split drivers");
        HDexit(EXIT_SUCCESS);
    }

#ifdef H5_HAVE_PARALLEL
    HDputs("Skip VFD SWMR test because paged aggregation is disabled in parallel HDF5");
    HDexit(EXIT_SUCCESS);
#endif

    /* Set up */
    h5_reset();

    if ((fapl = h5_fileaccess()) < 0) {
        nerrors++;
        PUTS_ERROR("Can't get VFD-dependent fapl")
    }

    /* Add nfs/updater testing in this routine */
    nerrors += test_fapl();

    if (use_file_locking) {
        nerrors += test_updater_flags();
        nerrors += test_updater_flags_same_file_opens();
#ifndef H5_HAVE_WIN32_API
        /* VFD SWMR: Fails on windows due to error from generate_md_ck_cb(). */
        nerrors += test_updater_generate_md_checksums(TRUE);
        nerrors += test_updater_generate_md_checksums(FALSE);
#endif

        nerrors += test_shadow_index_lookup();

        nerrors += test_file_fapl();
#ifndef H5_HAVE_WIN32_API
        /* XXX: VFD SWMR: Fails on Win32 due to problems unlinking the metadata file.
         *                The OS claims another process is using the file.
         */
        nerrors += test_writer_create_open_flush();
        nerrors += test_writer_md();
#endif
        nerrors += test_reader_md_concur();

        nerrors += test_multiple_file_opens();
        nerrors += test_multiple_file_opens_concur();
        nerrors += test_same_file_opens();

        nerrors += test_enable_disable_eot();
        nerrors += test_disable_enable_eot_concur();

        nerrors += test_file_end_tick();
        nerrors += test_file_end_tick_concur();
    }

    if (nerrors)
        goto error;

    HDputs("All VFD SWMR tests passed.");

    HDexit(EXIT_SUCCESS);

error:
    HDprintf("***** %d VFD SWMR TEST%s FAILED! *****\n", nerrors, nerrors > 1 ? "S" : "");

    H5E_BEGIN_TRY
    {
        H5Pclose(fapl);
    }
    H5E_END_TRY;

    HDexit(EXIT_FAILURE);
}
