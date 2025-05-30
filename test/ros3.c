/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the LICENSE file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Read-Only S3 Virtual File Driver (VFD)
 *
 * Purpose:
 *
 *     Verify behavior for Read-Only S3 VFD
 *     at the VFL (virtual file layer) level.
 *
 *     Demonstrates basic use cases and fapl/dxpl interaction.
 */

#include "h5test.h"

#include "H5FDprivate.h"      /* Virtual File Driver utilities */
#include "H5FDros3.h"         /* this file driver's utilities */
#include "H5FDros3_s3comms.h" /* for loading of credentials */

#ifdef H5_HAVE_ROS3_VFD

#define MAXADDR (((haddr_t)1 << (8 * sizeof(HDoff_t) - 1)) - 1)

#define S3_TEST_PROFILE_NAME "ros3_vfd_test"

/* Default region where the test files are located */
#define S3_TEST_DEFAULT_REGION "us-east-2"

#define S3_TEST_MAX_URL_SIZE 256

/* Size of buffer to allocate for session token */
#define S3_TEST_SESSION_TOKEN_SIZE 4097

#define S3_TEST_RESOURCE_TEXT_RESTRICTED "t8.shakespeare.txt"
#define S3_TEST_RESOURCE_TEXT_PUBLIC     "Poe_Raven.txt"
#define S3_TEST_RESOURCE_H5_PUBLIC       "GMODO-SVM01.h5"
#define S3_TEST_RESOURCE_MISSING         "missing.csv"

static char url_text_restricted[S3_TEST_MAX_URL_SIZE] = "";
static char url_text_public[S3_TEST_MAX_URL_SIZE]     = "";
static char url_h5_public[S3_TEST_MAX_URL_SIZE]       = "";
static char url_missing[S3_TEST_MAX_URL_SIZE]         = "";
static char s3_test_bucket_url[S3_TEST_MAX_URL_SIZE]  = "";
static bool s3_test_bucket_defined                    = false;

/* Global variables for aws test profile.
 * An attempt is made to read from environment variables and/or
 * ~/.aws/credentials and ~/.aws/config upon test startup --
 * if unable to open either file or cannot load id and key,
 * tests connecting with S3 will not be run
 */
static int   s3_test_credentials_loaded = 0;
static char  s3_test_aws_region[16];
static char  s3_test_aws_access_key_id[64];
static char  s3_test_aws_secret_access_key[128];
static char *s3_test_aws_session_token = NULL;

H5FD_ros3_fapl_t restricted_access_fa = {H5FD_CURR_ROS3_FAPL_T_VERSION, /* fapl version      */
                                         true,                          /* authenticate      */
                                         S3_TEST_DEFAULT_REGION,        /* aws region        */
                                         "",                            /* access key id     */
                                         ""};                           /* secret access key */

H5FD_ros3_fapl_t anonymous_fa = {H5FD_CURR_ROS3_FAPL_T_VERSION, false, S3_TEST_DEFAULT_REGION, "", ""};

H5FD_ros3_fapl_t empty_auth_fa   = {H5FD_CURR_ROS3_FAPL_T_VERSION, true, "", "", ""};
H5FD_ros3_fapl_t empty_id_fa     = {H5FD_CURR_ROS3_FAPL_T_VERSION, true, "where", "", ""};
H5FD_ros3_fapl_t empty_region_fa = {H5FD_CURR_ROS3_FAPL_T_VERSION, true, "", "me", ""};

/*---------------------------------------------------------------------------
 * Function:    test_fapl_config_validation
 *
 * Purpose:     Test ros3 fapl configurations and H5Pset/get_fapl_ros3()
 *
 * Return:      PASS : 0
 *              FAIL : 1
 *---------------------------------------------------------------------------
 */
static int
test_fapl_config_validation(void)
{
    struct testcase {
        const char      *msg;
        herr_t           expected;
        H5FD_ros3_fapl_t config;
    };

    hid_t           fapl_id     = H5I_INVALID_HID;
    const int       NCASES      = 5; /* Should equal number of cases */
    struct testcase cases_arr[] = {
        {
            "non-authenticating config allows empties.\n",
            SUCCEED,
            {
                H5FD_CURR_ROS3_FAPL_T_VERSION, /* version      */
                false,                         /* authenticate */
                "",                            /* aws_region   */
                "",                            /* secret_id    */
                "",                            /* secret_key   */
            },
        },
        {
            "populated strings; key is the empty string?\n",
            SUCCEED,
            {
                H5FD_CURR_ROS3_FAPL_T_VERSION,
                true,
                "region",
                "me",
                "",
            },
        },
        {
            "all strings populated.\n",
            SUCCEED,
            {
                H5FD_CURR_ROS3_FAPL_T_VERSION,
                true,
                "where",
                "who",
                "thisIsA GREAT seeeecrit",
            },
        },
        {
            "incorrect version should fail\n",
            FAIL,
            {
                12345,
                false,
                "",
                "",
                "",
            },
        },
        {
            "non-authenticating config cares not for (de)population"
            "of strings.\n",
            SUCCEED,
            {
                H5FD_CURR_ROS3_FAPL_T_VERSION,
                false,
                "someregion",
                "someid",
                "somekey",
            },
        },
    };

    TESTING("ros3 fapl configuration validation");

    if (false == s3_test_bucket_defined) {
        SKIPPED();
        puts("    environment variable HDF5_ROS3_TEST_BUCKET_URL not defined");
        fflush(stdout);
        return 0;
    }

    for (int i = 0; i < NCASES; i++) {

        struct testcase *case_ptr = &cases_arr[i]; /* Alias */
        herr_t           ret;

        if ((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
            TEST_ERROR;

        H5E_BEGIN_TRY
        {
            ret = H5Pset_fapl_ros3(fapl_id, &case_ptr->config);
        }
        H5E_END_TRY

        if (ret != case_ptr->expected)
            FAIL_PUTS_ERROR(case_ptr->msg);

        /* If H5Pset_fapl_ros3() succeeded, make sure H5Pget_fapl_ros3()
         * returns what we put in.
         */
        if (ret == SUCCEED) {
            H5FD_ros3_fapl_t config = case_ptr->config; /* Alias for this config */
            H5FD_ros3_fapl_t conf_out;                  /* Config from fapl */

            /* Get the config from the fapl */
            if (H5Pget_fapl_ros3(fapl_id, &conf_out) < 0)
                TEST_ERROR;

            /* Make sure all fields match */
            if (H5FD_CURR_ROS3_FAPL_T_VERSION != conf_out.version)
                FAIL_PUTS_ERROR("invalid version number");
            if (config.version != conf_out.version)
                FAIL_PUTS_ERROR("version number mismatch");
            if (config.authenticate != conf_out.authenticate)
                FAIL_PUTS_ERROR("authentication flag mismatch");
            if (strncmp(config.aws_region, conf_out.aws_region, H5FD_ROS3_MAX_REGION_LEN + 1))
                FAIL_PUTS_ERROR("AWS region mismatch");
            if (strncmp(config.secret_id, conf_out.secret_id, H5FD_ROS3_MAX_SECRET_ID_LEN + 1))
                FAIL_PUTS_ERROR("secret ID mismatch");
            if (strncmp(config.secret_key, conf_out.secret_key, H5FD_ROS3_MAX_SECRET_KEY_LEN + 1))
                FAIL_PUTS_ERROR("secret key mismatch");
        }

        if (H5Pclose(fapl_id) < 0)
            TEST_ERROR;
    }

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(fapl_id);
    }
    H5E_END_TRY
    return 1;
} /* end test_fapl_config_validation() */

/*-------------------------------------------------------------------------
 * Function:    test_ros3_fapl
 *
 * Purpose:     Tests the file handle interface for the ROS3 driver
 *
 *              As the ROS3 driver is 1) read only, 2) requires access
 *              to an S3 server, this test is quite
 *              different from the other tests.
 *
 *              For now, test only fapl & flags.  Extend as the
 *              work on the VFD continues.
 *
 * Return:      PASS : 0
 *              FAIL : 1
 *-------------------------------------------------------------------------
 */
static int
test_ros3_fapl_driver_flags(void)
{
    hid_t            fapl_id      = H5I_INVALID_HID; /* file access property list ID */
    hid_t            driver_id    = H5I_INVALID_HID; /* ID for this VFD              */
    unsigned long    driver_flags = 0;               /* VFD feature flags            */
    H5FD_ros3_fapl_t ros3_fa_0    = {
        H5FD_CURR_ROS3_FAPL_T_VERSION, /* version       */
        false,                         /* authenticate  */
        "",                            /* aws_region    */
        "",                            /* secret_id     */
        "plugh",                       /* secret_key    */
    };

    TESTING("ros3 driver flags");

    if ((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;
    if (H5Pset_fapl_ros3(fapl_id, &ros3_fa_0) < 0)
        TEST_ERROR;
    if ((driver_id = H5Pget_driver(fapl_id)) < 0)
        TEST_ERROR;

    /* Get VFD flags */
    if (H5FDdriver_query(driver_id, &driver_flags) < 0)
        TEST_ERROR;

    /* Validate flags */
    if (0 == (driver_flags & H5FD_FEAT_DATA_SIEVE))
        FAIL_PUTS_ERROR("ros3 VFD should support H5FD_FEAT_DATA_SIEVE");
    if (H5FD_FEAT_DATA_SIEVE != driver_flags)
        FAIL_PUTS_ERROR("H5FD_FEAT_DATA_SIEVE should be the only supported flag");

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(fapl_id);
    }
    H5E_END_TRY

    return 1;

} /* end test_ros3_fapl() */

/*---------------------------------------------------------------------------
 * Function:    test_vfl_open
 *
 * Purpose:     Test the VFL "open" callback
 *
 * Return:      PASS : 0
 *              FAIL : 1
 *---------------------------------------------------------------------------
 */
#define TESTS_COUNT 13
static int
test_vfl_open(void)
{
    struct test_condition {
        const char *message;
        const char *url;
        unsigned    flags;
        hid_t       fapl;
        haddr_t     maxaddr;
    };

    H5FD_t *fd                   = NULL;
    hid_t   ros3_fapl_id         = H5I_INVALID_HID;
    hid_t   default_fapl_id      = H5I_INVALID_HID;
    hid_t   empty_auth_fapl_id   = H5I_INVALID_HID;
    hid_t   empty_id_fapl_id     = H5I_INVALID_HID;
    hid_t   empty_region_fapl_id = H5I_INVALID_HID;

    struct test_condition tests[TESTS_COUNT] = {
        {
            "default property list (H5P_DEFAULT) is invalid",
            url_text_public,
            H5F_ACC_RDONLY,
            H5P_DEFAULT,
            MAXADDR,
        },
        {
            "generic file access property list is invalid",
            url_text_public,
            H5F_ACC_RDONLY,
            H5I_INVALID_HID,
            MAXADDR,
        },
        {
            "authenticating config asks for populated strings.\n",
            url_text_public,
            H5F_ACC_RDONLY,
            H5I_INVALID_HID,
            MAXADDR,
        },
        {
            "id cannot be empty.\n",
            url_text_public,
            H5F_ACC_RDONLY,
            H5I_INVALID_HID,
            MAXADDR,
        },
        {
            "region cannot be empty.\n",
            url_text_public,
            H5F_ACC_RDONLY,
            H5I_INVALID_HID,
            MAXADDR,
        },
        {
            "filename cannot be null",
            NULL,
            H5F_ACC_RDONLY,
            H5I_INVALID_HID,
            MAXADDR,
        },
        {
            "filename cannot be empty",
            "",
            H5F_ACC_RDONLY,
            H5I_INVALID_HID,
            MAXADDR,
        },
        {
            "filename must exist",
            url_missing,
            H5F_ACC_RDONLY,
            H5I_INVALID_HID,
            MAXADDR,
        },
        {
            "read-write flag not supported",
            url_text_public,
            H5F_ACC_RDWR,
            H5I_INVALID_HID,
            MAXADDR,
        },
        {
            "truncate flag not supported",
            url_text_public,
            H5F_ACC_TRUNC,
            H5I_INVALID_HID,
            MAXADDR,
        },
        {
            "create flag not supported",
            url_text_public,
            H5F_ACC_CREAT,
            H5I_INVALID_HID,
            MAXADDR,
        },
        {
            "EXCL flag not supported",
            url_text_public,
            H5F_ACC_EXCL,
            H5I_INVALID_HID,
            MAXADDR,
        },
        {
            "maxaddr cannot be 0 (caught in `H5FD_open()`)",
            url_text_public,
            H5F_ACC_RDONLY,
            H5I_INVALID_HID,
            0,
        },
    };

    TESTING("ros3 VFD-level open");

    if (false == s3_test_bucket_defined) {
        SKIPPED();
        puts("    environment variable HDF5_ROS3_TEST_BUCKET_URL not defined");
        fflush(stdout);
        return 0;
    }

    /* Set up fapls */
    if ((default_fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;
    if ((ros3_fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;
    if ((empty_auth_fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;
    if ((empty_id_fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;
    if ((empty_region_fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;
    if (H5Pset_fapl_ros3(ros3_fapl_id, &anonymous_fa) < 0)
        TEST_ERROR;
    if (H5Pset_fapl_ros3(empty_auth_fapl_id, &empty_auth_fa) < 0)
        TEST_ERROR;
    if (H5Pset_fapl_ros3(empty_id_fapl_id, &empty_id_fa) < 0)
        TEST_ERROR;
    if (H5Pset_fapl_ros3(empty_region_fapl_id, &empty_region_fa) < 0)
        TEST_ERROR;

    /* Set up test cases */
    tests[1].fapl = default_fapl_id;
    tests[2].fapl = empty_auth_fapl_id;
    tests[3].fapl = empty_id_fapl_id;
    tests[4].fapl = empty_region_fapl_id;
    for (int i = 5; i < TESTS_COUNT; i++)
        tests[i].fapl = ros3_fapl_id;

    /* Test a variety of cases that are expected to fail */
    for (int i = 0; i < TESTS_COUNT; i++) {

        H5E_BEGIN_TRY
        {
            fd = H5FDopen(tests[i].url, tests[i].flags, tests[i].fapl, tests[i].maxaddr);
        }
        H5E_END_TRY

        if (NULL != fd)
            FAIL_PUTS_ERROR(tests[i].message);
    }

    /* Finally, show that a file can be opened */
    if (NULL == (fd = H5FDopen(url_text_public, H5F_ACC_RDONLY, ros3_fapl_id, MAXADDR)))
        TEST_ERROR;
    if (H5FDclose(fd) < 0)
        TEST_ERROR;

    if (H5Pclose(empty_auth_fapl_id) < 0)
        TEST_ERROR;
    if (H5Pclose(empty_id_fapl_id) < 0)
        TEST_ERROR;
    if (H5Pclose(empty_region_fapl_id) < 0)
        TEST_ERROR;
    if (H5Pclose(default_fapl_id) < 0)
        TEST_ERROR;
    if (H5Pclose(ros3_fapl_id) < 0)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5FDclose(fd);
        H5Pclose(empty_auth_fapl_id);
        H5Pclose(empty_id_fapl_id);
        H5Pclose(empty_region_fapl_id);
        H5Pclose(default_fapl_id);
        H5Pclose(ros3_fapl_id);
    }
    H5E_END_TRY

    return 1;

} /* end test_vfd_open() */
#undef TESTS_COUNT

/*---------------------------------------------------------------------------
 * Function:    test_eof_eoa
 *
 * Purpose:     Demonstrate behavior of get_eof, get_eoa, and set_eoa
 *
 * Return:      PASS : 0
 *              FAIL : 1
 *---------------------------------------------------------------------------
 */
static int
test_eof_eoa(void)
{
    const haddr_t INITIAL_ADDR = 5458199; /* Fragile! */
    const haddr_t LOWER_ADDR   = INITIAL_ADDR - (1024 * 1024);
    const haddr_t HIGHER_ADDR  = INITIAL_ADDR + (1024 * 1024);
    H5FD_t       *fd           = NULL;
    hid_t         fapl_id      = H5I_INVALID_HID;

    TESTING("ros3 eof/eoa gets and sets");

    if (s3_test_credentials_loaded == 0) {
        SKIPPED();
        puts("    s3 credentials are not loaded");
        fflush(stdout);
        return 0;
    }

    if (false == s3_test_bucket_defined) {
        SKIPPED();
        puts("    environment variable HDF5_ROS3_TEST_BUCKET_URL not defined");
        fflush(stdout);
        return 0;
    }

    if ((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;
    if (H5Pset_fapl_ros3(fapl_id, &restricted_access_fa) < 0)
        TEST_ERROR;
    if (*s3_test_aws_session_token != '\0')
        if (H5Pset_fapl_ros3_token(fapl_id, s3_test_aws_session_token) < 0)
            TEST_ERROR;

    /* Open and verify EOA and EOF in a sample file */
    if (NULL == (fd = H5FDopen(url_text_restricted, H5F_ACC_RDONLY, fapl_id, HADDR_UNDEF)))
        TEST_ERROR;
    if (INITIAL_ADDR != H5FDget_eof(fd, H5FD_MEM_DEFAULT))
        FAIL_PUTS_ERROR("incorrect EOF (fragile - make sure the file size didn't change)");
    if (H5FDget_eof(fd, H5FD_MEM_DEFAULT) != H5FDget_eof(fd, H5FD_MEM_DRAW))
        FAIL_PUTS_ERROR("mismatch between DEFAULT and RAW memory types");
    if (0 != H5FDget_eoa(fd, H5FD_MEM_DEFAULT))
        FAIL_PUTS_ERROR("EOA should be unset by H5FDopen");

    /* Set EOA below EOF - should succeed w/ EOF changed and EOA unchanged */
    if (H5FDset_eoa(fd, H5FD_MEM_DEFAULT, LOWER_ADDR) < 0)
        TEST_ERROR;
    if (INITIAL_ADDR != H5FDget_eof(fd, H5FD_MEM_DEFAULT))
        FAIL_PUTS_ERROR("EOF changed when setting (lower) EOA");
    if (LOWER_ADDR != H5FDget_eoa(fd, H5FD_MEM_DEFAULT))
        FAIL_PUTS_ERROR("EOA unchanged when setting (lower) EOA");

    /* Set EOA above EOF - should succeed w/ EOF changed and EOA unchanged */
    if (H5FDset_eoa(fd, H5FD_MEM_DEFAULT, HIGHER_ADDR) < 0)
        TEST_ERROR;
    if (INITIAL_ADDR != H5FDget_eof(fd, H5FD_MEM_DEFAULT))
        FAIL_PUTS_ERROR("EOF changed when setting (higher) EOA");
    if (HIGHER_ADDR != H5FDget_eoa(fd, H5FD_MEM_DEFAULT))
        FAIL_PUTS_ERROR("EOA unchanged when setting (higher) EOA");

    if (H5FDclose(fd) < 0)
        TEST_ERROR;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5FDclose(fd);
        H5Pclose(fapl_id);
    }
    H5E_END_TRY

    return 1;

} /* end test_eof_eoa() */

/*---------------------------------------------------------------------------
 * Function:    test_vfl_read
 *
 * Purpose:     Test reading via the VFL API
 *
 * Return:      PASS : 0
 *              FAIL : 1
 *---------------------------------------------------------------------------
 */
static int
test_vfl_read(void)
{
    struct testcase {
        const char *message;  /* purpose of test case */
        haddr_t     eoa_set;  /* set file EOA to this prior to read */
        size_t      addr;     /* offset of read in file */
        size_t      len;      /* length of read in file */
        herr_t      success;  /* expected return value of read function */
        const char *expected; /* expected contents of buffer; failure ignores */
    };

    struct testcase tests[] = {
        {
            "successful range-get",
            6464,
            5691,
            32, /* fancy quotes are three bytes each(?) */
            SUCCEED,
            "Quoth the Raven “Nevermore.”",
        },
        {
            "read past EOA fails (EOA < EOF < addr)",
            3000,
            4000,
            100,
            FAIL,
            NULL,
        },
        {
            "read overlapping EOA fails (EOA < addr < EOF < (addr+len))",
            3000,
            8000,
            100,
            FAIL,
            NULL,
        },
        {
            "read past EOA/EOF fails ((EOA==EOF) < addr)",
            6464,
            7000,
            100,
            FAIL,
            NULL,
        },
        {
            "read overlapping EOA/EOF fails (addr < (EOA==EOF) < (addr+len))",
            6464,
            6400,
            100,
            FAIL,
            NULL,
        },
        {
            "read between EOF and EOA fails (EOF < addr < (addr+len) < EOA)",
            8000,
            7000,
            100,
            FAIL,
            NULL,
        },
    };
    const int TESTCASE_COUNT = 6;
    char      buffer[S3_TEST_MAX_URL_SIZE];
    H5FD_t   *fd      = NULL;
    hid_t     fapl_id = H5I_INVALID_HID;

    TESTING("ros3 VFD read/range-gets");

    if (s3_test_credentials_loaded == 0) {
        SKIPPED();
        puts("    s3 credentials are not loaded");
        fflush(stdout);
        return 0;
    }

    if (false == s3_test_bucket_defined) {
        SKIPPED();
        puts("    environment variable HDF5_ROS3_TEST_BUCKET_URL not defined");
        fflush(stdout);
        return 0;
    }

    if ((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;
    if (H5Pset_fapl_ros3(fapl_id, &restricted_access_fa) < 0)
        TEST_ERROR;
    if (*s3_test_aws_session_token != '\0')
        if (H5Pset_fapl_ros3_token(fapl_id, s3_test_aws_session_token) < 0)
            TEST_ERROR;

    if (NULL == (fd = H5FDopen(url_text_public, H5F_ACC_RDONLY, fapl_id, HADDR_UNDEF)))
        TEST_ERROR;
    if (6464 != H5FDget_eof(fd, H5FD_MEM_DEFAULT))
        FAIL_PUTS_ERROR("incorrect EOF (fragile - make sure the file size didn't change)");

    for (int i = 0; i < TESTCASE_COUNT; i++) {

        herr_t ret = FAIL;

        /* Per-test setup */

        if (S3_TEST_MAX_URL_SIZE < tests[i].len)
            FAIL_PUTS_ERROR("buffer too small!");
        if (H5FD_set_eoa(fd, H5FD_MEM_DEFAULT, tests[i].eoa_set) < 0)
            TEST_ERROR;
        memset(buffer, 0, S3_TEST_MAX_URL_SIZE);

        /* Check test config */

        H5E_BEGIN_TRY
        {
            ret = H5FDread(fd, H5FD_MEM_DRAW, H5P_DEFAULT, tests[i].addr, tests[i].len, buffer);
        }
        H5E_END_TRY

        if (tests[i].success != ret)
            FAIL_PUTS_ERROR(tests[i].message);
        if (ret == SUCCEED)
            if (strncmp(tests[i].expected, buffer, S3_TEST_MAX_URL_SIZE))
                FAIL_PUTS_ERROR("expected output is not the same");
    }

    if (H5FDclose(fd) < 0)
        TEST_ERROR;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5FDclose(fd);
        H5Pclose(fapl_id);
    }
    H5E_END_TRY

    return 1;

} /* end test_vfl_read() */

/*-----------------------------------------------------------------------------
 * Function:    test_vfl_read_without_eoa_set_fails
 *
 * Purpose:     Demonstrate a not-obvious constraint by the library, preventing
 *              file read before EOA is set
 *
 * Return:      PASS : 0
 *              FAIL : 1
 *-----------------------------------------------------------------------------
 */
static int
test_vfl_read_without_eoa_set_fails(void)
{
    char    buffer[256];
    H5FD_t *fd      = NULL;
    hid_t   fapl_id = H5I_INVALID_HID;
    herr_t  ret;

    TESTING("ros3 VFD read-eoa temporal coupling library limitation");

    if (s3_test_credentials_loaded == 0) {
        SKIPPED();
        puts("    s3 credentials are not loaded");
        fflush(stdout);
        return 0;
    }

    if (false == s3_test_bucket_defined) {
        SKIPPED();
        puts("    environment variable HDF5_ROS3_TEST_BUCKET_URL not defined");
        fflush(stdout);
        return 0;
    }

    /* Set up fapl */
    if ((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;
    if (H5Pset_fapl_ros3(fapl_id, &restricted_access_fa) < 0)
        TEST_ERROR;
    if (*s3_test_aws_session_token != '\0')
        if (H5Pset_fapl_ros3_token(fapl_id, s3_test_aws_session_token) < 0)
            TEST_ERROR;

    /* Open w/ VFL call */
    if (NULL == (fd = H5FDopen(url_text_restricted, H5F_ACC_RDONLY, fapl_id, MAXADDR)))
        TEST_ERROR;
    if (0 != H5FDget_eoa(fd, H5FD_MEM_DEFAULT))
        FAIL_PUTS_ERROR("EOA should remain unset by H5FDopen");

    /* Try reading without EOA set (should fail) */
    memset(buffer, 0, 256);
    H5E_BEGIN_TRY
    {
        ret = H5FDread(fd, H5FD_MEM_DRAW, H5P_DEFAULT, 1200699, 102, buffer);
    }
    H5E_END_TRY
    if (ret != FAIL)
        FAIL_PUTS_ERROR("should not be able to read before eoa is set");
    for (int i = 0; i < 256; i++) {
        if (buffer[i] != 0)
            FAIL_PUTS_ERROR("buffer should remain untouched");
    }

    if (H5FDclose(fd) < 0)
        TEST_ERROR;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5FDclose(fd);
        H5Pclose(fapl_id);
    }
    H5E_END_TRY

    return 1;

} /* end test_vfl_read_without_eoa_set_fails() */

/*---------------------------------------------------------------------------
 * Function:    test_noops_and_autofails
 *
 * Purpose:     Demonstrate the unavailable and do-nothing routines unique to
 *              Read-Only VFD
 *
 * Return:      PASS : 0
 *              FAIL : 1
 *---------------------------------------------------------------------------
 */
static int
test_noops_and_autofails(void)
{
    hid_t      fapl_id  = H5I_INVALID_HID;
    H5FD_t    *fd       = NULL;
    const char data[36] = "The Force shall be with you, always";
    herr_t     ret;

    TESTING("ros3 VFD always-fail and no-op routines");

    if (false == s3_test_bucket_defined) {
        SKIPPED();
        puts("    environment variable HDF5_ROS3_TEST_BUCKET_URL not defined");
        fflush(stdout);
        return 0;
    }

    if ((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;
    if (H5Pset_fapl_ros3(fapl_id, &anonymous_fa) < 0)
        TEST_ERROR;
    if (*s3_test_aws_session_token != '\0')
        if (H5Pset_fapl_ros3_token(fapl_id, s3_test_aws_session_token) < 0)
            TEST_ERROR;
    if (NULL == (fd = H5FDopen(url_text_public, H5F_ACC_RDONLY, fapl_id, HADDR_UNDEF)))
        TEST_ERROR;

    /* Calls to write and truncate must fail */
    H5E_BEGIN_TRY
    {
        ret = H5FDwrite(fd, H5FD_MEM_DRAW, H5P_DEFAULT, 1000, 35, data);
    }
    H5E_END_TRY
    if (ret == SUCCEED)
        FAIL_PUTS_ERROR("write calls must fail");

    H5E_BEGIN_TRY
    {
        ret = H5FDtruncate(fd, H5P_DEFAULT, false);
    }
    H5E_END_TRY
    if (ret == SUCCEED)
        FAIL_PUTS_ERROR("truncate calls must fail");

    H5E_BEGIN_TRY
    {
        ret = H5FDtruncate(fd, H5P_DEFAULT, true);
    }
    H5E_END_TRY
    if (ret == SUCCEED)
        FAIL_PUTS_ERROR("truncate calls must fail (closing flag set)");

    if (H5FDclose(fd) < 0)
        TEST_ERROR;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5FDclose(fd);
        H5Pclose(fapl_id);
    }
    H5E_END_TRY

    return 1;

} /* end test_noops_and_autofails() */

/*---------------------------------------------------------------------------
 * Function:    test_cmp
 *
 * Purpose:     Verify file comparison behavior
 *
 * Return:      PASS : 0
 *              FAIL : 1
 *---------------------------------------------------------------------------
 */
static int
test_cmp(void)
{
    H5FD_t *fd_raven   = NULL;
    H5FD_t *fd_shakes  = NULL;
    H5FD_t *fd_raven_2 = NULL;
    hid_t   fapl_id    = H5I_INVALID_HID;

    TESTING("ros3 cmp (comparison)");

    if (s3_test_credentials_loaded == 0) {
        SKIPPED();
        puts("    s3 credentials are not loaded");
        fflush(stdout);
        return 0;
    }

    if (false == s3_test_bucket_defined) {
        SKIPPED();
        puts("    environment variable HDF5_ROS3_TEST_BUCKET_URL not defined");
        fflush(stdout);
        return 0;
    }

    if ((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;
    if (H5Pset_fapl_ros3(fapl_id, &restricted_access_fa) < 0)
        TEST_ERROR;
    if (*s3_test_aws_session_token != '\0')
        if (H5Pset_fapl_ros3_token(fapl_id, s3_test_aws_session_token) < 0)
            TEST_ERROR;

    /* Open files */
    if (NULL == (fd_raven = H5FDopen(url_text_public, H5F_ACC_RDONLY, fapl_id, HADDR_UNDEF)))
        TEST_ERROR;
    if (NULL == (fd_shakes = H5FDopen(url_text_restricted, H5F_ACC_RDONLY, fapl_id, HADDR_UNDEF)))
        TEST_ERROR;
    if (NULL == (fd_raven_2 = H5FDopen(url_text_public, H5F_ACC_RDONLY, fapl_id, HADDR_UNDEF)))
        TEST_ERROR;

    /* Compare files */
    if (0 != H5FDcmp(fd_raven, fd_raven_2))
        FAIL_PUTS_ERROR("bad comparison (case 1)");
    if (-1 != H5FDcmp(fd_raven, fd_shakes))
        FAIL_PUTS_ERROR("bad comparison (case 2)");
    if (-1 != H5FDcmp(fd_shakes, fd_raven_2))
        FAIL_PUTS_ERROR("bad comparison (case 3)");

    if (H5FDclose(fd_raven) < 0)
        TEST_ERROR;
    if (H5FDclose(fd_shakes) < 0)
        TEST_ERROR;
    if (H5FDclose(fd_raven_2) < 0)
        TEST_ERROR;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5FDclose(fd_raven);
        H5FDclose(fd_shakes);
        H5FDclose(fd_raven_2);
        H5Pclose(fapl_id);
    }
    H5E_END_TRY

    return 1;

} /* end test_cmp() */

/*---------------------------------------------------------------------------
 * Function:    test_ros3_access_modes
 *
 * Purpose:     Make sure ros3 files can only be opened read-only
 *
 * Return:      PASS : 0
 *              FAIL : 1
 *---------------------------------------------------------------------------
 */
static int
test_ros3_access_modes(void)
{
    hid_t fid     = H5I_INVALID_HID;
    hid_t fapl_id = H5I_INVALID_HID;

    TESTING("ros3 access modes");

    if (s3_test_credentials_loaded == 0) {
        SKIPPED();
        puts("    s3 credentials are not loaded");
        fflush(stdout);
        return 0;
    }

    if (false == s3_test_bucket_defined) {
        SKIPPED();
        puts("    environment variable HDF5_ROS3_TEST_BUCKET_URL not defined");
        fflush(stdout);
        return 0;
    }

    if ((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;
    if (H5Pset_fapl_ros3(fapl_id, &restricted_access_fa) < 0)
        TEST_ERROR;
    if (*s3_test_aws_session_token != '\0')
        if (H5Pset_fapl_ros3_token(fapl_id, s3_test_aws_session_token) < 0)
            TEST_ERROR;

    /* Read-Write Open access is not allowed with this file driver */
    H5E_BEGIN_TRY
    {
        fid = H5Fopen(url_h5_public, H5F_ACC_RDWR, fapl_id);
    }
    H5E_END_TRY
    if (fid != H5I_INVALID_HID)
        FAIL_PUTS_ERROR("should not be allowed to open a file read-write with the ros3 VFD");

    /* H5Fcreate() is not allowed with this file driver */
    H5E_BEGIN_TRY
    {
        fid = H5Fcreate(url_missing, H5F_ACC_RDONLY, H5P_DEFAULT, fapl_id);
    }
    H5E_END_TRY
    if (fid != H5I_INVALID_HID)
        FAIL_PUTS_ERROR("should not be allowed to create a file with the ros3 VFD");

    /* Read-only access should succeed */
    if ((fid = H5Fopen(url_h5_public, H5F_ACC_RDONLY, fapl_id)) < 0)
        TEST_ERROR;

    if (H5Fclose(fid) < 0)
        TEST_ERROR;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Fclose(fid);
        H5Pclose(fapl_id);
    }
    H5E_END_TRY

    return 1;

} /* end test_ros3_access_modes() */
#endif /* H5_HAVE_ROS3_VFD */

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Tests the basic functionality of the ros3 VFD
 *
 * Return:      Success:    EXIT_SUCCESS
 *              Failure:    EXIT_FAILURE
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    int ret_value = EXIT_SUCCESS;

#ifdef H5_HAVE_ROS3_VFD
    int         nerrors           = 0;
    const char *bucket_url_env    = NULL;
    bool        credentials_found = false;
#endif /* H5_HAVE_ROS3_VFD */

    printf("Testing ros3 VFD functionality.\n");

#ifdef H5_HAVE_ROS3_VFD

    /************************
     * Initialize test urls *
     ************************/

    bucket_url_env = getenv("HDF5_ROS3_TEST_BUCKET_URL");
    if (bucket_url_env == NULL || bucket_url_env[0] == '\0') {
        printf("WARNING: S3 bucket url is not defined in environment "
               "variable 'HDF5_ROS3_TEST_BUCKET_URL'!\n");
    }
    else {
        strncpy(s3_test_bucket_url, bucket_url_env, S3_TEST_MAX_URL_SIZE);
        s3_test_bucket_url[S3_TEST_MAX_URL_SIZE - 1] = '\0';
        s3_test_bucket_defined                       = true;
    }

    if (S3_TEST_MAX_URL_SIZE < snprintf(url_text_restricted, (size_t)S3_TEST_MAX_URL_SIZE, "%s/%s",
                                        (const char *)s3_test_bucket_url,
                                        (const char *)S3_TEST_RESOURCE_TEXT_RESTRICTED)) {
        printf("* ros3 setup failed (text_restricted) ! *\n");
        ret_value = EXIT_FAILURE;
        goto done;
    }
    if (S3_TEST_MAX_URL_SIZE < snprintf(url_text_public, (size_t)S3_TEST_MAX_URL_SIZE, "%s/%s",
                                        (const char *)s3_test_bucket_url,
                                        (const char *)S3_TEST_RESOURCE_TEXT_PUBLIC)) {
        printf("* ros3 setup failed (text_public) ! *\n");
        ret_value = EXIT_FAILURE;
        goto done;
    }
    if (S3_TEST_MAX_URL_SIZE < snprintf(url_h5_public, (size_t)S3_TEST_MAX_URL_SIZE, "%s/%s",
                                        (const char *)s3_test_bucket_url,
                                        (const char *)S3_TEST_RESOURCE_H5_PUBLIC)) {
        printf("* ros3 setup failed (h5_public) ! *\n");
        ret_value = EXIT_FAILURE;
        goto done;
    }
    if (S3_TEST_MAX_URL_SIZE < snprintf(url_missing, S3_TEST_MAX_URL_SIZE, "%s/%s",
                                        (const char *)s3_test_bucket_url,
                                        (const char *)S3_TEST_RESOURCE_MISSING)) {
        printf("* ros3 setup failed (missing) ! *\n");
        ret_value = EXIT_FAILURE;
        goto done;
    }

    /**************************************
     * Load credentials and prepare fapls *
     **************************************/

    /* Clear profile data strings */
    s3_test_aws_access_key_id[0]     = '\0';
    s3_test_aws_secret_access_key[0] = '\0';
    s3_test_aws_region[0]            = '\0';

    if (NULL == (s3_test_aws_session_token = malloc(S3_TEST_SESSION_TOKEN_SIZE))) {
        fprintf(stderr, "couldn't allocate buffer for session token\n");
        ret_value = EXIT_FAILURE;
        goto done;
    }

    /* Attempt to load test credentials - if unable, certain tests will be skipped */
    if (h5_load_aws_environment(
            &credentials_found, s3_test_aws_access_key_id, sizeof(s3_test_aws_access_key_id),
            s3_test_aws_secret_access_key, sizeof(s3_test_aws_secret_access_key), s3_test_aws_region,
            sizeof(s3_test_aws_region), s3_test_aws_session_token, S3_TEST_SESSION_TOKEN_SIZE) < 0) {
        fprintf(stderr, "error occurred while trying to load AWS credentials\n");
        ret_value = EXIT_FAILURE;
        goto done;
    }

    if (credentials_found) {
        if (s3_test_aws_access_key_id[0] != '\0' && s3_test_aws_secret_access_key[0] != '\0') {
            s3_test_credentials_loaded = 1;
            strncpy(restricted_access_fa.secret_id, s3_test_aws_access_key_id, H5FD_ROS3_MAX_SECRET_ID_LEN);
            strncpy(restricted_access_fa.secret_key, s3_test_aws_secret_access_key,
                    H5FD_ROS3_MAX_SECRET_KEY_LEN);
            if (s3_test_aws_region[0] != '\0') {
                strncpy(restricted_access_fa.aws_region, s3_test_aws_region, H5FD_ROS3_MAX_REGION_LEN);
                strncpy(anonymous_fa.aws_region, s3_test_aws_region, H5FD_ROS3_MAX_REGION_LEN);
            }
        }
        else {
            /* Clear profile data strings */
            s3_test_aws_access_key_id[0]     = '\0';
            s3_test_aws_secret_access_key[0] = '\0';
            s3_test_aws_region[0]            = '\0';
            s3_test_aws_session_token[0]     = '\0';
            credentials_found                = false;
        }
    }

    if (!credentials_found) {
        if (h5_load_aws_profile(S3_TEST_PROFILE_NAME, &credentials_found, s3_test_aws_access_key_id,
                                sizeof(s3_test_aws_access_key_id), s3_test_aws_secret_access_key,
                                sizeof(s3_test_aws_secret_access_key), s3_test_aws_region,
                                sizeof(s3_test_aws_region)) < 0) {
            fprintf(stderr, "error occurred while trying to load AWS credentials\n");
            ret_value = EXIT_FAILURE;
            goto done;
        }

        if (credentials_found) {
            if (s3_test_aws_access_key_id[0] != '\0' && s3_test_aws_secret_access_key[0] != '\0') {
                s3_test_credentials_loaded = 1;
                strncpy(restricted_access_fa.secret_id, s3_test_aws_access_key_id,
                        H5FD_ROS3_MAX_SECRET_ID_LEN);
                strncpy(restricted_access_fa.secret_key, s3_test_aws_secret_access_key,
                        H5FD_ROS3_MAX_SECRET_KEY_LEN);
                if (s3_test_aws_region[0] != '\0') {
                    strncpy(restricted_access_fa.aws_region, s3_test_aws_region, H5FD_ROS3_MAX_REGION_LEN);
                    strncpy(anonymous_fa.aws_region, s3_test_aws_region, H5FD_ROS3_MAX_REGION_LEN);
                }
            }
        }
    }

    /******************
     * Commence tests *
     ******************/

    h5_test_init();

    if (H5FD__s3comms_init() < 0) {
        fprintf(stderr, "failed to initialize s3 communications interface\n");
        ret_value = EXIT_FAILURE;
        goto done;
    }

    if (nerrors == 0) {
        nerrors += test_fapl_config_validation();
        nerrors += test_ros3_fapl_driver_flags();
        nerrors += test_vfl_open();
        nerrors += test_eof_eoa();
        nerrors += test_vfl_read();
        nerrors += test_vfl_read_without_eoa_set_fails();
        nerrors += test_noops_and_autofails();
        nerrors += test_cmp();
        nerrors += test_ros3_access_modes();
    }

    if (H5FD__s3comms_term() < 0) {
        fprintf(stderr, "failed to terminate s3 communications interface\n");
        ret_value = EXIT_FAILURE;
        goto done;
    }

    if (nerrors > 0) {
        printf("***** %d ros3 TEST%s FAILED! *****\n", nerrors, nerrors > 1 ? "S" : "");
        ret_value = EXIT_FAILURE;
        goto done;
    }

    printf("All ros3 tests passed.\n");

done:
    free(s3_test_aws_session_token);

#else

    printf("SKIPPED - read-only S3 VFD not built\n");

#endif /* H5_HAVE_ROS3_VFD */

    exit(ret_value);
} /* end main() */
