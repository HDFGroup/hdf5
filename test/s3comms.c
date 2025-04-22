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
 * Purpose:    Unit tests for the S3 Communications (s3comms) module.
 */

#include "h5test.h"

#include "H5FDros3_s3comms.h"

#ifdef H5_HAVE_ROS3_VFD

#define S3_TEST_PROFILE_NAME "ros3_vfd_test"

#define S3_TEST_RESOURCE_TEXT_RESTRICTED "t8.shakespeare.txt"
#define S3_TEST_RESOURCE_TEXT_PUBLIC     "Poe_Raven.txt"
#define S3_TEST_RESOURCE_MISSING         "missing.csv"

/* URL max size */
#define S3_TEST_MAX_URL_SIZE 256

/* Read buffer max size */
#define S3COMMS_READ_BUFFER_SIZE 256

/* Global variables for AWS test profile.
 *
 * An attempt is made to read ~/.aws/credentials and ~/.aws/config upon test
 * startup -- if unable to open either file or cannot load region, id, and key,
 * tests connecting with S3 will not be run.
 */
static int  s3_test_credentials_loaded               = 0;
static char s3_test_aws_region[16]                   = "";
static char s3_test_aws_access_key_id[64]            = "";
static char s3_test_aws_secret_access_key[128]       = "";
static char s3_test_aws_security_token[1024]         = "";
static char s3_test_bucket_url[S3_TEST_MAX_URL_SIZE] = "";
static bool s3_test_bucket_defined                   = false;

/*---------------------------------------------------------------------------
 * Function:    test_s3r_get_filesize
 *
 * Purpose:     Test H5FD__s3comms_s3r_get_filesize()
 *
 * Return:      PASS : 0
 *              FAIL : 1
 *---------------------------------------------------------------------------
 */
static int
test_s3r_get_filesize(void)
{
    H5FD_ros3_fapl_t anonymous_fa = {H5FD_CURR_ROS3_FAPL_T_VERSION, false, "us-east-2", "", ""};
    char             url_raven[S3_TEST_MAX_URL_SIZE];
    s3r_t           *handle = NULL;

    TESTING("s3r_get_filesize");

    /* Setup -- compose url to target resource */
    if (false == s3_test_bucket_defined) {
        SKIPPED();
        puts("    environment variable HDF5_ROS3_TEST_BUCKET_URL not defined");
        fflush(stdout);
        return 0;
    }

    if (S3_TEST_MAX_URL_SIZE <
        snprintf(url_raven, S3_TEST_MAX_URL_SIZE, "%s/%s", s3_test_bucket_url, S3_TEST_RESOURCE_TEXT_PUBLIC))
        TEST_ERROR;

    if (0 != H5FD__s3comms_s3r_get_filesize(NULL))
        FAIL_PUTS_ERROR("filesize of the null handle should be 0");

    if (NULL == (handle = H5FD__s3comms_s3r_open(url_raven, &anonymous_fa, NULL)))
        TEST_ERROR;

    if (6464 != H5FD__s3comms_s3r_get_filesize(handle))
        FAIL_PUTS_ERROR("incorrect file size - fragile, make sure the file size didn't change");

    if (H5FD__s3comms_s3r_close(handle) < 0)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    if (handle != NULL)
        H5FD__s3comms_s3r_close(handle);
    return 1;
} /* end test_s3r_get_filesize() */

/*---------------------------------------------------------------------------
 * Function:    test_s3r_open
 *
 * Purpose:     Test H5FD__s3comms_s3r_open()
 *
 * Return:      PASS : 0
 *              FAIL : 1
 *---------------------------------------------------------------------------
 */
static int
test_s3r_open(void)
{
    char              url_missing[S3_TEST_MAX_URL_SIZE];
    char              url_raven[S3_TEST_MAX_URL_SIZE];
    char              url_shakespeare[S3_TEST_MAX_URL_SIZE];
    H5FD_ros3_fapl_t *fa     = NULL;
    s3r_t            *handle = NULL;

    TESTING("s3r_open");

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

    /******************
     * PRE-TEST SETUP *
     ******************/

    /* Create and fill a common fapl
     *
     * Specific fields will be set (and reset) as needed by tests below
     */
    if (NULL == (fa = (H5FD_ros3_fapl_t *)calloc(1, sizeof(H5FD_ros3_fapl_t))))
        TEST_ERROR;
    fa->version      = H5FD_CURR_ROS3_FAPL_T_VERSION;
    fa->authenticate = true;
    strcpy(fa->aws_region, s3_test_aws_region);
    strcpy(fa->secret_id, s3_test_aws_access_key_id);
    strcpy(fa->secret_key, s3_test_aws_secret_access_key);

    if (S3_TEST_MAX_URL_SIZE < snprintf(url_shakespeare, S3_TEST_MAX_URL_SIZE, "%s/%s", s3_test_bucket_url,
                                        S3_TEST_RESOURCE_TEXT_RESTRICTED))
        TEST_ERROR;

    if (S3_TEST_MAX_URL_SIZE <
        snprintf(url_missing, S3_TEST_MAX_URL_SIZE, "%s/%s", s3_test_bucket_url, S3_TEST_RESOURCE_MISSING))
        TEST_ERROR;

    if (S3_TEST_MAX_URL_SIZE <
        snprintf(url_raven, S3_TEST_MAX_URL_SIZE, "%s/%s", s3_test_bucket_url, S3_TEST_RESOURCE_TEXT_PUBLIC))
        TEST_ERROR;

    /*************************
     * OPEN NONEXISTENT FILE *
     *************************/

    /* Attempt anonymously */
    H5E_BEGIN_TRY
    {
        handle = H5FD__s3comms_s3r_open(url_missing, NULL, NULL);
    }
    H5E_END_TRY
    if (handle != NULL)
        TEST_ERROR;

    /* Attempt with authentication */
    H5E_BEGIN_TRY
    {
        handle = H5FD__s3comms_s3r_open(url_missing, fa, (const char *)s3_test_aws_security_token);
    }
    H5E_END_TRY
    if (handle != NULL)
        TEST_ERROR;

    /*******************************
     * INVALID AUTHENTICATION INFO *
     *******************************/

    /* Anonymous access on restricted file */
    H5E_BEGIN_TRY
    {
        handle = H5FD__s3comms_s3r_open(url_shakespeare, NULL, NULL);
    }
    H5E_END_TRY
    if (handle != NULL)
        TEST_ERROR;

    /* Pass in a bad ID */
    strcpy(fa->secret_id, "I_MADE_UP_MY_ID");
    H5E_BEGIN_TRY
    {
        handle = H5FD__s3comms_s3r_open(url_shakespeare, fa, (const char *)s3_test_aws_security_token);
    }
    H5E_END_TRY
    if (handle != NULL)
        TEST_ERROR;
    strcpy(fa->secret_id, s3_test_aws_access_key_id);

    /* Using an invalid signing key */
    strcpy(fa->secret_key, "I_AM_A_FAKE_KEY");
    H5E_BEGIN_TRY
    {
        handle = H5FD__s3comms_s3r_open(url_shakespeare, fa, (const char *)s3_test_aws_security_token);
    }
    H5E_END_TRY
    if (handle != NULL)
        TEST_ERROR;
    strcpy(fa->secret_key, s3_test_aws_secret_access_key);

    /*******************************
     * SUCCESSFUL OPEN (AND CLOSE) *
     *******************************/

    /* Anonymous */
    handle = H5FD__s3comms_s3r_open(url_raven, NULL, NULL);
    if (handle == NULL)
        TEST_ERROR;
    if (6464 != H5FD__s3comms_s3r_get_filesize(handle))
        FAIL_PUTS_ERROR("did not get expected filesize");
    if (H5FD__s3comms_s3r_close(handle) < 0)
        TEST_ERROR;
    handle = NULL;

    /* Using authentication on anonymously-accessible file? */
    handle = H5FD__s3comms_s3r_open(url_raven, fa, (const char *)s3_test_aws_security_token);
    if (handle == NULL)
        TEST_ERROR;
    if (6464 != H5FD__s3comms_s3r_get_filesize(handle))
        FAIL_PUTS_ERROR("did not get expected filesize");
    if (H5FD__s3comms_s3r_close(handle))
        TEST_ERROR;
    handle = NULL;

    /* Authenticating */
    handle = H5FD__s3comms_s3r_open(url_shakespeare, fa, (const char *)s3_test_aws_security_token);
    if (handle == NULL)
        TEST_ERROR;
    if (5458199 != H5FD__s3comms_s3r_get_filesize(handle))
        FAIL_PUTS_ERROR("did not get expected filesize");
    if (H5FD__s3comms_s3r_close(handle) < 0)
        TEST_ERROR;
    handle = NULL;

    free(fa);

    PASSED();
    return 0;
error:
    if (handle != NULL)
        H5FD__s3comms_s3r_close(handle);
    free(fa);

    return 1;
} /* end test_s3r_open() */

/*---------------------------------------------------------------------------
 * Function:    test_s3r_read
 *
 * Purpose:     Specify and demonstrate the use and life cycle of an S3
 *              request handle `s3r_t`, through its related functions.
 *
 *     H5FD__s3comms_s3r_open
 *     H5FD_s3comms_s3r_getsize << called by open() _only_
 *     H5FD__s3comms_s3r_read    << called by getsize(), multiple times working
 *     H5FD__s3comms_s3r_close
 *
 *     Shows most basic curl iteration
 *
 * Return:      PASS : 0
 *              FAIL : 1
 *---------------------------------------------------------------------------
 */
static int
test_s3r_read(void)
{
    H5FD_ros3_fapl_t anonymous_fa = {H5FD_CURR_ROS3_FAPL_T_VERSION, false, "us-east-2", "", ""};
    char             url_raven[S3_TEST_MAX_URL_SIZE];
    char             buffer[S3COMMS_READ_BUFFER_SIZE];
    s3r_t           *handle = NULL;
    herr_t           ret;

    TESTING("s3r_read");

    /* Initial setup */
    if (false == s3_test_bucket_defined) {
        SKIPPED();
        puts("    environment variable HDF5_ROS3_TEST_BUCKET_URL not defined");
        fflush(stdout);
        return 0;
    }

    if (S3_TEST_MAX_URL_SIZE <
        snprintf(url_raven, S3_TEST_MAX_URL_SIZE, "%s/%s", s3_test_bucket_url, S3_TEST_RESOURCE_TEXT_PUBLIC))
        TEST_ERROR;

    /* Open file */
    handle = H5FD__s3comms_s3r_open(url_raven, &anonymous_fa, NULL);
    if (handle == NULL)
        TEST_ERROR;
    if (6464 != H5FD__s3comms_s3r_get_filesize(handle))
        TEST_ERROR;

    /*****************************
     * Tests that should succeed *
     *****************************/

    /* Read from start of file */
    memset(buffer, 0, S3COMMS_READ_BUFFER_SIZE);
    if (H5FD__s3comms_s3r_read(handle, (haddr_t)0, (size_t)118, buffer, S3COMMS_READ_BUFFER_SIZE) < 0)
        TEST_ERROR;
    if (strcmp("Once upon a midnight dreary, while I pondered, weak and weary,\n"
               "Over many a quaint and curious volume of forgotten lore",
               buffer))
        TEST_ERROR;

    /* Read arbitrary range */
    memset(buffer, 0, S3COMMS_READ_BUFFER_SIZE);
    if (H5FD__s3comms_s3r_read(handle, (haddr_t)2540, (size_t)54, buffer, S3COMMS_READ_BUFFER_SIZE) < 0)
        TEST_ERROR;
    if (strcmp("the grave and stern decorum of the countenance it wore", buffer))
        TEST_ERROR;

    /* Read one character */
    memset(buffer, 0, S3COMMS_READ_BUFFER_SIZE);
    if (H5FD__s3comms_s3r_read(handle, (haddr_t)2540, (size_t)1, buffer, S3COMMS_READ_BUFFER_SIZE) < 0)
        TEST_ERROR;
    if (strcmp("t", buffer))
        TEST_ERROR;

    /* Read to EOF */
    memset(buffer, 0, S3COMMS_READ_BUFFER_SIZE);
    if (H5FD__s3comms_s3r_read(handle, (haddr_t)6370, (size_t)0, buffer, S3COMMS_READ_BUFFER_SIZE) < 0)
        TEST_ERROR;
    if (strncmp(
            buffer,
            "And my soul from out that shadow that lies floating on the floor\nShall be lifted—nevermore!\n",
            94))
        TEST_ERROR;

    /**************************
     * Tests that should fail *
     **************************/

    /* Read past eof */
    memset(buffer, 0, S3COMMS_READ_BUFFER_SIZE);
    H5E_BEGIN_TRY
    {
        ret = H5FD__s3comms_s3r_read(handle, (haddr_t)6400, (size_t)100, /* 6400+100 > 6464 */ buffer,
                                     S3COMMS_READ_BUFFER_SIZE);
    }
    H5E_END_TRY
    if (ret == SUCCEED)
        TEST_ERROR;
    if (strcmp("", buffer))
        TEST_ERROR;

    /* Read starts past eof */
    memset(buffer, 0, S3COMMS_READ_BUFFER_SIZE);
    H5E_BEGIN_TRY
    {
        ret = H5FD__s3comms_s3r_read(handle, (haddr_t)1200699, /* 1200699 > 6464 */ (size_t)100, buffer,
                                     S3COMMS_READ_BUFFER_SIZE);
    }
    H5E_END_TRY
    if (ret == SUCCEED)
        TEST_ERROR;
    if (strcmp("", buffer))
        TEST_ERROR;

    /* Read starts on eof */
    memset(buffer, 0, S3COMMS_READ_BUFFER_SIZE);
    H5E_BEGIN_TRY
    {
        ret = H5FD__s3comms_s3r_read(handle, (haddr_t)6464, (size_t)0, buffer, S3COMMS_READ_BUFFER_SIZE);
    }
    H5E_END_TRY
    if (ret == SUCCEED)
        TEST_ERROR;
    if (strcmp("", buffer))
        TEST_ERROR;

    /*************
     * TEAR DOWN *
     *************/

    if (H5FD__s3comms_s3r_close(handle) < 0)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    if (handle != NULL)
        H5FD__s3comms_s3r_close(handle);
    return 1;
} /* end test_s3r_read() */

#endif /* H5_HAVE_ROS3_VFD */

/*-------------------------------------------------------------------------
 * Function:    main()
 *
 * Purpose:     Run unit tests for S3 communications (s3comms)
 *
 * Return:      EXIT_SUCCESS/EXIT_FAILURE
 *-------------------------------------------------------------------------
 */
int
main(void)
{
#ifdef H5_HAVE_ROS3_VFD
    int         nerrors            = 0;
    const char *bucket_url_env     = NULL;
    bool        test_profile_found = false;

    h5_test_init();

#endif /* H5_HAVE_ROS3_VFD */

    printf("Testing S3 communications functionality\n");

#ifdef H5_HAVE_ROS3_VFD

    /* "clear" profile data strings */
    s3_test_aws_access_key_id[0]     = '\0';
    s3_test_aws_secret_access_key[0] = '\0';
    s3_test_aws_region[0]            = '\0';
    s3_test_bucket_url[0]            = '\0';

    /* attempt to load test credentials
     * if unable, certain tests will be skipped
     */
    if (h5_load_aws_profile(S3_TEST_PROFILE_NAME, &test_profile_found, s3_test_aws_access_key_id,
                            sizeof(s3_test_aws_access_key_id), s3_test_aws_secret_access_key,
                            sizeof(s3_test_aws_secret_access_key), s3_test_aws_region,
                            sizeof(s3_test_aws_region)) < 0) {
        fprintf(stderr, "error occurred while trying to load AWS credentials\n");
        return EXIT_FAILURE;
    }

    if (test_profile_found) {
        if (s3_test_aws_access_key_id[0] != '\0' && s3_test_aws_secret_access_key[0] != '\0' &&
            s3_test_aws_region[0] != '\0') {
            s3_test_credentials_loaded = 1;
        }
    }

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

    if (H5FD__s3comms_init() < 0) {
        fprintf(stderr, "failed to initialize s3 communications interface\n");
        return EXIT_FAILURE;
    }

    nerrors += test_s3r_get_filesize();
    nerrors += test_s3r_open();
    nerrors += test_s3r_read();

    if (H5FD__s3comms_term() < 0) {
        fprintf(stderr, "failed to terminate s3 communications interface\n");
        return EXIT_FAILURE;
    }

    if (nerrors) {
        printf("***** %d s3comms TEST%s FAILED! *****\n", nerrors, nerrors > 1 ? "S" : "");
        return EXIT_FAILURE;
    }

    printf("All s3comms tests passed.\n");
    return EXIT_SUCCESS;

#else

    printf("SKIPPED - read-only S3 VFD not built\n");
    return EXIT_SUCCESS;

#endif /* H5_HAVE_ROS3_VFD */

} /* end main() */
