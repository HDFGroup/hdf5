/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
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
 * Read-Only S3 Virtual File Driver (VFD)
 *
 * Purpose:
 *
 *     Verify behavior for Read-Only S3 VFD
 *     at the VFL (virtual file layer) level.
 *
 *     Demonstrates basic use cases and fapl/dxpl interaction.
 *
 * Programmer: Jacob Smith <jake.smith@hdfgroup.org>
 *             2017-10-11
 */

#include "h5test.h"

#include "H5FDprivate.h" /* Virtual File Driver utilities */
#include "H5FDros3.h"    /* this file driver's utilities */
#include "H5FDs3comms.h" /* for loading of credentials */

#ifdef H5_HAVE_ROS3_VFD

/* only include the testing macros if needed */

/*****************************************************************************
 *
 * FILE-LOCAL TESTING MACROS
 *
 * Purpose:
 *
 *     1) Upon test failure, goto-jump to single-location teardown in test
 *        function. E.g., `error:` (consistency with HDF corpus) or
 *        `failed:` (reflects purpose).
 *            >>> using "error", in part because `H5E_BEGIN_TRY` expects it.
 *     2) Increase clarity and reduce overhead found with `TEST_ERROR`.
 *        e.g., "if(somefunction(arg, arg2) < 0) TEST_ERROR:"
 *        requires reading of entire line to know whether this if/call is
 *        part of the test setup, test operation, or a test unto itself.
 *     3) Provide testing macros with optional user-supplied failure message;
 *        if not supplied (NULL), generate comparison output in the spirit of
 *        test-driven development. E.g., "expected 5 but was -3"
 *        User messages clarify test's purpose in code, encouraging description
 *        without relying on comments.
 *     4) Configurable expected-actual order in generated comparison strings.
 *        Some prefer `VERIFY(expected, actual)`, others
 *        `VERIFY(actual, expected)`. Provide preprocessor ifdef switch
 *        to satifsy both parties, assuming one paradigm per test file.
 *        (One could #undef and redefine the flag through the file as desired,
 *         but _why_.)
 *
 *     Provided as courtesy, per consideration for inclusion in the library
 *     proper.
 *
 *     Macros:
 *
 *         JSVERIFY_EXP_ACT - ifdef flag, configures comparison order
 *         FAIL_IF()        - check condition
 *         FAIL_UNLESS()    - check _not_ condition
 *         JSVERIFY()       - long-int equality check; prints reason/comparison
 *         JSVERIFY_NOT()   - long-int inequality check; prints
 *         JSVERIFY_STR()   - string equality check; prints
 *
 * Programmer: Jacob Smith
 *             2017-10-24
 *
 *****************************************************************************/


/*----------------------------------------------------------------------------
 *
 * ifdef flag: JSVERIFY_EXP_ACT
 *
 * JSVERIFY macros accept arguments as (EXPECTED, ACTUAL[, reason])
 *   default, if this is undefined, is (ACTUAL, EXPECTED[, reason])
 *
 *----------------------------------------------------------------------------
 */
#define JSVERIFY_EXP_ACT 1L


/*----------------------------------------------------------------------------
 *
 * Macro: JSFAILED_AT()
 *
 * Purpose:
 *
 *     Preface a test failure by printing "*FAILED*" and location to stdout
 *     Similar to `H5_FAILED(); AT();` from h5test.h
 *
 *     *FAILED* at somefile.c:12 in function_name()...
 *
 * Programmer: Jacob Smith
 *             2017-10-24
 *
 *----------------------------------------------------------------------------
 */
#define JSFAILED_AT() {                                                   \
    HDprintf("*FAILED* at %s:%d in %s()...\n", __FILE__, __LINE__, FUNC); \
}


/*----------------------------------------------------------------------------
 *
 * Macro: FAIL_IF()
 *
 * Purpose:
 *
 *     Make tests more accessible and less cluttered than
 *         `if (thing == otherthing()) TEST_ERROR`
 *         paradigm.
 *
 *     The following lines are roughly equivalent:
 *
 *         `if (myfunc() < 0) TEST_ERROR;` (as seen elsewhere in HDF tests)
 *         `FAIL_IF(myfunc() < 0)`
 *
 *     Prints a generic "FAILED AT" line to stdout and jumps to `error`,
 *     similar to `TEST_ERROR` in h5test.h
 *
 * Programmer: Jacob Smith
 *             2017-10-23
 *
 *----------------------------------------------------------------------------
 */
#define FAIL_IF(condition) \
if (condition) {           \
    JSFAILED_AT()          \
    goto error;           \
}


/*----------------------------------------------------------------------------
 *
 * Macro: FAIL_UNLESS()
 *
 * Purpose:
 *
 *     TEST_ERROR wrapper to reduce cognitive overhead from "negative tests",
 *     e.g., "a != b".
 *
 *     Opposite of FAIL_IF; fails if the given condition is _not_ true.
 *
 *     `FAIL_IF( 5 != my_op() )`
 *     is equivalent to
 *     `FAIL_UNLESS( 5 == my_op() )`
 *     However, `JSVERIFY(5, my_op(), "bad return")` may be even clearer.
 *         (see JSVERIFY)
 *
 * Programmer: Jacob Smith
 *             2017-10-24
 *
 *----------------------------------------------------------------------------
 */
#if 0 /* UNUSED */
#define FAIL_UNLESS(condition) \
if (!(condition)) {            \
    JSFAILED_AT()              \
    goto error;                \
}
#endif


/*----------------------------------------------------------------------------
 *
 * Macro: JSERR_LONG()
 *
 * Purpose:
 *
 *     Print an failure message for long-int arguments.
 *     ERROR-AT printed first.
 *     If `reason` is given, it is printed on own line and newlined after
 *     else, prints "expected/actual" aligned on own lines.
 *
 *     *FAILED* at myfile.c:488 in somefunc()...
 *     forest must be made of trees.
 *
 *     or
 *
 *     *FAILED* at myfile.c:488 in somefunc()...
 *       ! Expected 425
 *       ! Actual   3
 *
 * Programmer: Jacob Smith
 *             2017-10-24
 *
 *----------------------------------------------------------------------------
 */
#define JSERR_LONG(expected, actual, reason) {           \
    JSFAILED_AT()                                        \
    if (reason!= NULL) {                                 \
        HDprintf("%s\n", (reason));                      \
    } else {                                             \
        HDprintf("  ! Expected %ld\n  ! Actual   %ld\n", \
                  (long)(expected), (long)(actual));     \
    }                                                    \
}


/*----------------------------------------------------------------------------
 *
 * Macro: JSERR_STR()
 *
 * Purpose:
 *
 *     Print an failure message for string arguments.
 *     ERROR-AT printed first.
 *     If `reason` is given, it is printed on own line and newlined after
 *     else, prints "expected/actual" aligned on own lines.
 *
 *     *FAILED*  at myfile.c:421 in myfunc()...
 *     Blue and Red strings don't match!
 *
 *     or
 *
 *     *FAILED*  at myfile.c:421 in myfunc()...
 *     !!! Expected:
 *     this is my expected
 *     string
 *     !!! Actual:
 *     not what I expected at all
 *
 * Programmer: Jacob Smith
 *             2017-10-24
 *
 *----------------------------------------------------------------------------
 */
#define JSERR_STR(expected, actual, reason) {           \
    JSFAILED_AT()                                       \
    if ((reason) != NULL) {                             \
        HDprintf("%s\n", (reason));                     \
    } else {                                            \
        HDprintf("!!! Expected:\n%s\n!!!Actual:\n%s\n", \
                 (expected), (actual));                 \
    }                                                   \
}



#ifdef JSVERIFY_EXP_ACT


/*----------------------------------------------------------------------------
 *
 * Macro: JSVERIFY()
 *
 * Purpose:
 *
 *     Verify that two long integers are equal.
 *     If unequal, print failure message
 *     (with `reason`, if not NULL; expected/actual if NULL)
 *     and jump to `error` at end of function
 *
 * Programmer: Jacob Smith
 *             2017-10-24
 *
 *----------------------------------------------------------------------------
 */
#define JSVERIFY(expected, actual, reason)     \
if ((long)(actual) != (long)(expected)) {      \
    JSERR_LONG((expected), (actual), (reason)) \
    goto error;                                \
} /* JSVERIFY */


/*----------------------------------------------------------------------------
 *
 * Macro: JSVERIFY_NOT()
 *
 * Purpose:
 *
 *     Verify that two long integers are _not_ equal.
 *     If equal, print failure message
 *     (with `reason`, if not NULL; expected/actual if NULL)
 *     and jump to `error` at end of function
 *
 * Programmer: Jacob Smith
 *             2017-10-24
 *
 *----------------------------------------------------------------------------
 */
#define JSVERIFY_NOT(expected, actual, reason) \
if ((long)(actual) == (long)(expected)) {      \
    JSERR_LONG((expected), (actual), (reason)) \
    goto error;                                \
} /* JSVERIFY_NOT */


/*----------------------------------------------------------------------------
 *
 * Macro: JSVERIFY_STR()
 *
 * Purpose:
 *
 *     Verify that two strings are equal.
 *     If unequal, print failure message
 *     (with `reason`, if not NULL; expected/actual if NULL)
 *     and jump to `error` at end of function
 *
 * Programmer: Jacob Smith
 *             2017-10-24
 *
 *----------------------------------------------------------------------------
 */
#define JSVERIFY_STR(expected, actual, reason) \
if (strcmp((actual), (expected)) != 0) {       \
    JSERR_STR((expected), (actual), (reason)); \
    goto error;                                \
} /* JSVERIFY_STR */


#else
/* JSVERIFY_EXP_ACT not defined
 *
 * Repeats macros above, but with actual/expected parameters reversed.
 */


/*----------------------------------------------------------------------------
 * Macro: JSVERIFY()
 * See: JSVERIFY documentation above.
 * Programmer: Jacob Smith
 *             2017-10-14
 *----------------------------------------------------------------------------
 */
#define JSVERIFY(actual, expected, reason)      \
if ((long)(actual) != (long)(expected)) {       \
    JSERR_LONG((expected), (actual), (reason)); \
    goto error;                                 \
} /* JSVERIFY */


/*----------------------------------------------------------------------------
 * Macro: JSVERIFY_NOT()
 * See: JSVERIFY_NOT documentation above.
 * Programmer: Jacob Smith
 *             2017-10-14
 *----------------------------------------------------------------------------
 */
#define JSVERIFY_NOT(actual, expected, reason) \
if ((long)(actual) == (long)(expected)) {      \
    JSERR_LONG((expected), (actual), (reason)) \
    goto error;                                \
} /* JSVERIFY_NOT */


/*----------------------------------------------------------------------------
 * Macro: JSVERIFY_STR()
 * See: JSVERIFY_STR documentation above.
 * Programmer: Jacob Smith
 *             2017-10-14
 *----------------------------------------------------------------------------
 */
#define JSVERIFY_STR(actual, expected, reason) \
if (strcmp((actual), (expected)) != 0) {       \
    JSERR_STR((expected), (actual), (reason)); \
    goto error;                                \
} /* JSVERIFY_STR */

#endif /* ifdef/else JSVERIFY_EXP_ACT */

/********************************
 * OTHER MACROS AND DEFINITIONS *
 ********************************/

#define MAXADDR (((haddr_t)1<<(8*sizeof(HDoff_t)-1))-1)

#define S3_TEST_PROFILE_NAME "ros3_vfd_test"

#define S3_TEST_MAX_URL_SIZE 256

#define S3_TEST_RESOURCE_TEXT_RESTRICTED "t8.shakespeare.txt"
#define S3_TEST_RESOURCE_TEXT_PUBLIC "Poe_Raven.txt"
#define S3_TEST_RESOURCE_H5_PUBLIC "GMODO-SVM01.h5"
#define S3_TEST_RESOURCE_MISSING "missing.csv"

static char    url_text_restricted[S3_TEST_MAX_URL_SIZE] = "";
static char    url_text_public[S3_TEST_MAX_URL_SIZE]     = "";
static char    url_h5_public[S3_TEST_MAX_URL_SIZE]       = "";
static char    url_missing[S3_TEST_MAX_URL_SIZE]         = "";
static char    s3_test_bucket_url[S3_TEST_MAX_URL_SIZE]  = "";
static hbool_t s3_test_bucket_defined                    = FALSE;

/* Global variables for aws test profile.
 * An attempt is made to read ~/.aws/credentials and ~/.aws/config upon test
 * startup -- if unable to open either file or cannot load region, id, and key,
 * tests connecting with S3 will not be run
 */
static int  s3_test_credentials_loaded = 0;
static char s3_test_aws_region[16];
static char s3_test_aws_access_key_id[64];
static char s3_test_aws_secret_access_key[128];

H5FD_ros3_fapl_t restricted_access_fa = {
            H5FD_CURR_ROS3_FAPL_T_VERSION, /* fapl version      */
            TRUE,                           /* authenticate      */
            "",             /* aws region        */
            "",      /* access key id     */
            ""}; /* secret access key */

H5FD_ros3_fapl_t anonymous_fa = {
            H5FD_CURR_ROS3_FAPL_T_VERSION,
            FALSE, "", "", "" };


/*---------------------------------------------------------------------------
 *
 * Function: test_fapl_config_validation()
 *
 * Purpose:
 *
 *     Test data consistency of fapl configuration.
 *     Tests `H5FD_ros3_validate_config` indirectly through `H5Pset_fapl_ros3`.
 *
 * Return:
 *
 *     PASSED : 0
 *     FAILED : 1
 *
 * Programmer: Jacob Smith
 *             2017-10-23
 *
 *---------------------------------------------------------------------------
 */
static int
test_fapl_config_validation(void)
{

    /*********************
     * test-local macros *
     *********************/

    /*************************
     * test-local structures *
     *************************/

    struct testcase {
        const char       *msg;
        herr_t            expected;
        H5FD_ros3_fapl_t  config;
    };

    /************************
     * test-local variables *
     ************************/

    hid_t            fapl_id     = -1;   /* file access property list ID */
    H5FD_ros3_fapl_t config;
    H5FD_ros3_fapl_t fa_fetch;
    herr_t           success     = SUCCEED;
    unsigned int     i           = 0;
    unsigned int     ncases      = 8;    /* should equal number of cases */
    struct testcase *case_ptr    = NULL; /* dumb work-around for possible     */
                                         /* dynamic cases creation because    */
                                         /* of compiler warnings Wlarger-than */
    struct testcase  cases_arr[] = {
        {   "non-authenticating config allows empties.\n",
            SUCCEED,
            {   H5FD_CURR_ROS3_FAPL_T_VERSION, /* version      */
                FALSE,                          /* authenticate */
                "",                             /* aws_region   */
                "",                             /* secret_id    */
                "",                             /* secret_key   */
            },
        },
        {   "authenticating config asks for populated strings.\n",
            FAIL,
            {   H5FD_CURR_ROS3_FAPL_T_VERSION,
                TRUE,
                "",
                "",
                "",
            },
        },
        {   "populated strings; key is the empty string?\n",
            SUCCEED,
            {   H5FD_CURR_ROS3_FAPL_T_VERSION,
                TRUE,
                "region",
                "me",
                "",
            },
        },
        {   "id cannot be empty.\n",
            FAIL,
            {   H5FD_CURR_ROS3_FAPL_T_VERSION,
                TRUE,
                "",
                "me",
                "",
            },
        },
        {   "region cannot be empty.\n",
            FAIL,
            {   H5FD_CURR_ROS3_FAPL_T_VERSION,
                TRUE,
                "where",
                "",
                "",
            },
        },
        {   "all strings populated.\n",
            SUCCEED,
            {   H5FD_CURR_ROS3_FAPL_T_VERSION,
                TRUE,
                "where",
                "who",
                "thisIsA GREAT seeeecrit",
            },
        },
        {   "incorrect version should fail\n",
            FAIL,
            {   12345,
                FALSE,
                "",
                "",
                "",
            },
        },
        {   "non-authenticating config cares not for (de)population"
            "of strings.\n",
            SUCCEED,
            {   H5FD_CURR_ROS3_FAPL_T_VERSION,
                FALSE,
                "someregion",
                "someid",
                "somekey",
            },
        },
    };

    TESTING("ROS3 fapl configuration validation");

    /*********
     * TESTS *
     *********/

    if (FALSE == s3_test_bucket_defined) {
        SKIPPED();
        puts("    environment variable HDF5_ROS3_TEST_BUCKET_URL not defined");
        fflush(stdout);
        return 0;
    }

    for (i = 0; i < ncases; i++) {

        /*---------------
         * per-test setup
         *---------------
         */
        case_ptr = &cases_arr[i];
        fapl_id = H5Pcreate(H5P_FILE_ACCESS);
        FAIL_IF( fapl_id < 0 ) /* sanity-check */

        /*-----------------------------------
         * Actually test.
         * Mute stack trace in failure cases.
         *-----------------------------------
         */
        H5E_BEGIN_TRY {
            /* `H5FD_ros3_validate_config(...)` is static/private
             * to src/ros3.c and cannot (and should not?) be tested directly?
             * Instead, validate config through public api.
             */
            success = H5Pset_fapl_ros3(fapl_id, &case_ptr->config);
        } H5E_END_TRY;

        JSVERIFY( case_ptr->expected, success, case_ptr->msg )

        /* Make sure we can get back what we put in.
         * Only valid if the fapl configuration does not result in error.
         */
        if (success == SUCCEED) {
            config = case_ptr->config;
            JSVERIFY( SUCCEED,
                      H5Pget_fapl_ros3(fapl_id, &fa_fetch),
                      "unable to get fapl" )

            JSVERIFY( H5FD_CURR_ROS3_FAPL_T_VERSION,
                      fa_fetch.version,
                      "invalid version number" )
            JSVERIFY( config.version,
                      fa_fetch.version,
                      "version number mismatch" )
            JSVERIFY( config.authenticate,
                      fa_fetch.authenticate,
                      "authentication flag mismatch" )
            JSVERIFY_STR( config.aws_region,
                          fa_fetch.aws_region,
                          NULL )
            JSVERIFY_STR( config.secret_id,
                          fa_fetch.secret_id,
                          NULL )
            JSVERIFY_STR( config.secret_key,
                          fa_fetch.secret_key,
                          NULL )
        }

        /*-----------------------------
         * per-test sanitation/teardown
         *-----------------------------
         */
        FAIL_IF( FAIL == H5Pclose(fapl_id) )
        fapl_id = -1;

    } /* for each test case */

    PASSED();
    return 0;

error:
    /***********
     * CLEANUP *
     ***********/

    if (fapl_id < 0) {
        H5E_BEGIN_TRY {
            (void)H5Pclose(fapl_id);
        } H5E_END_TRY;
    }
    return 1;
} /* test_fapl_config_validation */


/*-------------------------------------------------------------------------
 *
 * Function:    test_ros3_fapl()
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
 * Return:      Success:        0
 *              Failure:        1
 *
 * Programmer:  John Mainzer
 *              7/12/17
 *
 *-------------------------------------------------------------------------
 */
static int
test_ros3_fapl(void)
{
    /************************
     * test-local variables *
     ************************/

    hid_t             fapl_id        = -1;  /* file access property list ID */
    hid_t             driver_id      = -1;  /* ID for this VFD              */
    unsigned long     driver_flags   =  0;  /* VFD feature flags            */
    H5FD_ros3_fapl_t  ros3_fa_0      = {
        H5FD_CURR_ROS3_FAPL_T_VERSION, /* version       */
        FALSE,                          /* authenticate  */
        "",                             /* aws_region    */
        "",                             /* secret_id     */
        "plugh",                        /* secret_key    */
    };

    TESTING("ROS3 fapl ");

    /* Set property list and file name for ROS3 driver.
     */
    fapl_id = H5Pcreate(H5P_FILE_ACCESS);
    FAIL_IF( fapl_id < 0 )

    FAIL_IF( FAIL == H5Pset_fapl_ros3(fapl_id, &ros3_fa_0) )

    driver_id = H5Pget_driver(fapl_id);
    FAIL_IF( driver_id < 0 )

    /****************
     * Check that the VFD feature flags are correct
     * SPEC MAY CHANGE
     ******************/

    FAIL_IF( H5FDdriver_query(driver_id, &driver_flags) < 0 )

    JSVERIFY_NOT( 0, (driver_flags & H5FD_FEAT_DATA_SIEVE),
                  "bit(s) in `driver_flags` must align with "
                  "H5FD_FEAT_DATA_SIEVE" )

    JSVERIFY( H5FD_FEAT_DATA_SIEVE, driver_flags,
              "H5FD_FEAT_DATA_SIEVE should be the only supported flag")

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        (void)H5Pclose(fapl_id);
    } H5E_END_TRY;

    return 1;

} /* test_ros3_fapl() */


/*---------------------------------------------------------------------------
 *
 * Function: test_vfd_open()
 *
 * Purpose:
 *
 *     Demonstrate/specify VFD-level "Open" failure cases
 *
 * Return:
 *
 *     PASSED : 0
 *     FAILED : 1
 *
 * Programmer: Jacob Smith
 *             1027-11-03
 *
 *---------------------------------------------------------------------------
 */
static int
test_vfd_open(void)
{

    /*********************
     * test-local macros *
     *********************/


#define FAPL_H5P_DEFAULT -2
#define FAPL_FILE_ACCESS -3
#define FAPL_ROS3_ANON   -4

    /*************************
     * test-local structures *
     *************************/

    struct test_condition {
        const char *message;
        const char *url;
        unsigned    flags;
        int         which_fapl;
        haddr_t     maxaddr;
    };

    /************************
     * test-local variables *
     ************************/

    struct test_condition tests[] = {
        {   "default property list (H5P_DEFAULT) is invalid",
            url_text_public,
            H5F_ACC_RDONLY,
            FAPL_H5P_DEFAULT,
            MAXADDR,
        },
        {   "generic file access property list is invalid",
            url_text_public,
            H5F_ACC_RDONLY,
            FAPL_FILE_ACCESS,
            MAXADDR,
        },
        {   "filename cannot be null",
            NULL,
            H5F_ACC_RDONLY,
            FAPL_ROS3_ANON,
            MAXADDR,
        },
        {   "filename cannot be empty",
            "",
            H5F_ACC_RDONLY,
            FAPL_ROS3_ANON,
            MAXADDR,
        },
        {   "filename must exist",
            url_missing,
            H5F_ACC_RDONLY,
            FAPL_ROS3_ANON,
            MAXADDR,
        },
        {   "read-write flag not supported",
            url_text_public,
            H5F_ACC_RDWR,
            FAPL_ROS3_ANON,
            MAXADDR,
        },
        {   "truncate flag not supported",
            url_text_public,
            H5F_ACC_TRUNC,
            FAPL_ROS3_ANON,
            MAXADDR,
        },
        {   "create flag not supported",
            url_text_public,
            H5F_ACC_CREAT,
            FAPL_ROS3_ANON,
            MAXADDR,
        },
        {   "EXCL flag not supported",
            url_text_public,
            H5F_ACC_EXCL,
            FAPL_ROS3_ANON,
            MAXADDR,
        },
        {   "maxaddr cannot be 0 (caught in `H5FD_open()`)",
            url_text_public,
            H5F_ACC_RDONLY,
            FAPL_ROS3_ANON,
            0,
        },
    };
    H5FD_t   *fd         = NULL;
    hbool_t   curl_ready = FALSE;
    hid_t     fapl_id    = -1;
    hid_t     fapl_file_access = -1;
    unsigned  i                = 0;
    unsigned  tests_count      = 10;

    TESTING("ROS3 VFD-level open");

    if (FALSE == s3_test_bucket_defined) {
        SKIPPED();
        puts("    environment variable HDF5_ROS3_TEST_BUCKET_URL not defined");
        fflush(stdout);
        return 0;
    }

    FAIL_IF( CURLE_OK != curl_global_init(CURL_GLOBAL_DEFAULT) )
    curl_ready = TRUE;

    fapl_file_access = H5Pcreate(H5P_FILE_ACCESS);
    FAIL_IF( fapl_file_access < 0 )

    fapl_id = H5Pcreate(H5P_FILE_ACCESS);
    FAIL_IF( fapl_id < 0 )
    FAIL_IF( FAIL == H5Pset_fapl_ros3(fapl_id, &anonymous_fa) )

    /*********
     * TESTS *
     *********/

    /* all the test cases that will _not_ open
     */
    for (i = 0; i < tests_count; i++) {
        struct test_condition T = tests[i];
        hid_t _fapl_id = H5P_DEFAULT;

        fd = NULL;

        if (T.which_fapl == FAPL_FILE_ACCESS)
            _fapl_id = fapl_file_access;
        else if (T.which_fapl == FAPL_ROS3_ANON)
            _fapl_id = fapl_id;

        H5E_BEGIN_TRY {
            fd = H5FDopen(T.url, T.flags, _fapl_id, T.maxaddr);
        } H5E_END_TRY;
        if (NULL != fd)
            JSVERIFY(1, 0, T.message); /* wrapper to print message and fail */
    }

    FAIL_IF( NULL != fd )

    /* finally, show that a file can be opened
     */
    fd = H5FDopen(
            url_text_public,
            H5F_ACC_RDONLY,
            fapl_id,
            MAXADDR);
    FAIL_IF( NULL == fd )

    /************
     * TEARDOWN *
     ************/

    FAIL_IF( FAIL == H5FDclose(fd) )
    fd = NULL;

    FAIL_IF( FAIL == H5Pclose(fapl_id) )
    fapl_id = -1;

    FAIL_IF( FAIL == H5Pclose(fapl_file_access) )
    fapl_file_access = -1;

    curl_global_cleanup();
    curl_ready = FALSE;

    PASSED();
    return 0;

error:
    /***********
     * CLEANUP *
     ***********/

    if (fd) {
        (void)H5FDclose(fd);
    }
    if (fapl_id >= 0) {
        H5E_BEGIN_TRY {
            (void)H5Pclose(fapl_id);
        } H5E_END_TRY;
    }
    if (fapl_file_access >= 0) {
        H5E_BEGIN_TRY {
            (void)H5Pclose(fapl_file_access);
        } H5E_END_TRY;
    }
    if (curl_ready == TRUE) {
        curl_global_cleanup();
    }

    return 1;

#undef FAPL_FILE_ACCESS
#undef FAPL_H5P_DEFAULT
#undef FAPL_ROS3_ANON

} /* test_vfd_open */


/*---------------------------------------------------------------------------
 *
 * Function: test_eof_eoa()
 *
 * Purpose:
 *
 *     Demonstrate behavior of get_eof, get_eoa, and set_eoa.
 *
 * Return:
 *
 *     PASSED : 0
 *     FAILED : 1
 *
 * Programmer: Jacob Smith
 *             2017-11-08
 *
 *---------------------------------------------------------------------------
 */
static int
test_eof_eoa(void)
{

    /*********************
     * test-local macros *
     *********************/

    /*************************
     * test-local structures *
     *************************/

    /************************
     * test-local variables *
     ************************/

    H5FD_t  *fd_shakespeare  = NULL;
    hbool_t  curl_ready      = FALSE;
    hid_t    fapl_id         = -1;

    TESTING("ROS3 eof/eoa gets and sets");

    if (s3_test_credentials_loaded == 0) {
        SKIPPED();
        puts("    s3 credentials are not loaded");
        fflush(stdout);
        return 0;
    }

    if (FALSE == s3_test_bucket_defined) {
        SKIPPED();
        puts("    environment variable HDF5_ROS3_TEST_BUCKET_URL not defined");
        fflush(stdout);
        return 0;
    }

    /*********
     * SETUP *
     *********/

    FAIL_IF( CURLE_OK != curl_global_init(CURL_GLOBAL_DEFAULT) )
    curl_ready = TRUE;

    fapl_id = H5Pcreate(H5P_FILE_ACCESS);
    FAIL_IF( 0 > fapl_id )
    FAIL_IF( FAIL == H5Pset_fapl_ros3(fapl_id, &restricted_access_fa) )

    fd_shakespeare = H5FDopen(
             url_text_restricted,
             H5F_ACC_RDONLY,
             fapl_id,
             HADDR_UNDEF);
    FAIL_IF( NULL == fd_shakespeare )

    /*********
     * TESTS *
     *********/

    /* verify as found
     */
    JSVERIFY( 5458199, H5FDget_eof(fd_shakespeare, H5FD_MEM_DEFAULT), NULL )
    JSVERIFY( H5FDget_eof(fd_shakespeare, H5FD_MEM_DEFAULT),
              H5FDget_eof(fd_shakespeare, H5FD_MEM_DRAW),
              "mismatch between DEFAULT and RAW memory types" )
    JSVERIFY( 0,
              H5FDget_eoa(fd_shakespeare, H5FD_MEM_DEFAULT),
              "EoA should be unset by H5FDopen" )

    /* set EoA below EoF
     */
    JSVERIFY( SUCCEED,
              H5FDset_eoa(fd_shakespeare, H5FD_MEM_DEFAULT, 44442202),
              "unable to set EoA (lower)" )
    JSVERIFY( 5458199,
              H5FDget_eof(fd_shakespeare, H5FD_MEM_DEFAULT),
              "EoF changed" )
    JSVERIFY( 44442202,
              H5FDget_eoa(fd_shakespeare, H5FD_MEM_DEFAULT),
              "EoA unchanged" )

    /* set EoA above EoF
     */
    JSVERIFY( SUCCEED,
              H5FDset_eoa(fd_shakespeare, H5FD_MEM_DEFAULT, 6789012),
              "unable to set EoA (higher)" )
    JSVERIFY( 5458199,
              H5FDget_eof(fd_shakespeare, H5FD_MEM_DEFAULT),
              "EoF changed" )
    JSVERIFY( 6789012,
              H5FDget_eoa(fd_shakespeare, H5FD_MEM_DEFAULT),
              "EoA unchanged" )

    /************
     * TEARDOWN *
     ************/

    FAIL_IF( FAIL == H5FDclose(fd_shakespeare) )

    FAIL_IF( FAIL == H5Pclose(fapl_id) )
    fapl_id = -1;

    curl_global_cleanup();
    curl_ready = FALSE;

    PASSED();
    return 0;

error:
    /***********
     * CLEANUP *
     ***********/

    if (fd_shakespeare)     (void)H5FDclose(fd_shakespeare);
    if (TRUE == curl_ready) curl_global_cleanup();
    if (fapl_id >= 0) {
        H5E_BEGIN_TRY {
            (void)H5Pclose(fapl_id);
        } H5E_END_TRY;
    }

    return 1;

} /* test_eof_eoa */


/*-----------------------------------------------------------------------------
 *
 * Function: test_H5FDread_without_eoa_set_fails()
 *
 * Purpose:
 *
 *     Demonstrate a not-obvious constraint by the library, preventing
 *     file read before EoA is set
 *
 * Programmer: Jacob Smith
 *             2018-01-26
 *
 *-----------------------------------------------------------------------------
 */
static int
test_H5FDread_without_eoa_set_fails(void)
{
    char              buffer[256];
    unsigned int      i                = 0;
    H5FD_t           *file_shakespeare = NULL;
    hid_t             fapl_id          = -1;

    TESTING("ROS3 VFD read-eoa temporal coupling library limitation ");

    if (s3_test_credentials_loaded == 0) {
        SKIPPED();
        puts("    s3 credentials are not loaded");
        fflush(stdout);
        return 0;
    }

    if (FALSE == s3_test_bucket_defined) {
        SKIPPED();
        puts("    environment variable HDF5_ROS3_TEST_BUCKET_URL not defined");
        fflush(stdout);
        return 0;
    }

    /*********
     * SETUP *
     *********/

    /* create ROS3 fapl
     */
    fapl_id = H5Pcreate(H5P_FILE_ACCESS);
    FAIL_IF( fapl_id < 0 )
    FAIL_IF( FAIL == H5Pset_fapl_ros3(fapl_id, &restricted_access_fa) )

    file_shakespeare = H5FDopen(
            url_text_restricted,
            H5F_ACC_RDONLY,
            fapl_id,
            MAXADDR);
    FAIL_IF( NULL == file_shakespeare )

    JSVERIFY( 0, H5FDget_eoa(file_shakespeare, H5FD_MEM_DEFAULT),
              "EoA should remain unset by H5FDopen" )

    for (i = 0; i < 256; i++)
        buffer[i] = 0; /* zero buffer contents */

    /********
     * TEST *
     ********/

    H5E_BEGIN_TRY { /* mute stack trace on expected failure */
        JSVERIFY( FAIL,
                  H5FDread(file_shakespeare,
                       H5FD_MEM_DRAW,
                       H5P_DEFAULT,
                       1200699,
                       102,
                       buffer),
                  "cannot read before eoa is set" )
    } H5E_END_TRY;
    JSVERIFY_STR( "", buffer, "buffer should remain untouched" )

    /************
     * TEARDOWN *
     ************/

    FAIL_IF( FAIL == H5FDclose(file_shakespeare) )
    file_shakespeare = NULL;

    FAIL_IF( FAIL == H5Pclose(fapl_id) )
    fapl_id = -1;

    PASSED();
    return 0;

error:
    /***********
     * CLEANUP *
     ***********/

    if (file_shakespeare)   { (void)H5FDclose(file_shakespeare); }
    if (fapl_id >= 0) {
        H5E_BEGIN_TRY {
           (void)H5Pclose(fapl_id);
        } H5E_END_TRY;
    }

    return 1;

} /* test_H5FDread_without_eoa_set_fails */



/*---------------------------------------------------------------------------
 *
 * Function: test_read()
 *
 * Purpose:
 *
 * Return:
 *
 *     PASSED : 0
 *     FAILED : 1
 *
 * Programmer: Jacob Smith
 *             2017-11-06
 *
 *---------------------------------------------------------------------------
 */
static int
test_read(void)
{

    /*********************
     * test-local macros *
     *********************/

    /*************************
     * test-local structures *
     *************************/
    struct testcase {
        const char *message;  /* purpose of test case */
        haddr_t     eoa_set;  /* set file EOA to this prior to read */
        size_t      addr;     /* offset of read in file */
        size_t      len;      /* length of read in file */
        herr_t      success;  /* expected return value of read function */
        const char *expected; /* expected contents of buffer; failure ignores */
    };

    /************************
     * test-local variables *
     ************************/
    struct testcase cases[] = {
        {   "successful range-get",
            6464,
            5691,
            32, /* fancy quotes are three bytes each(?) */
            SUCCEED,
            "Quoth the Raven “Nevermore.”",
        },
        {   "read past EOA fails (EOA < EOF < addr)",
            3000,
            4000,
            100,
            FAIL,
            NULL,
        },
        {   "read overlapping EOA fails (EOA < addr < EOF < (addr+len))",
            3000,
            8000,
            100,
            FAIL,
            NULL,
        },
        {   "read past EOA/EOF fails ((EOA==EOF) < addr)",
            6464,
            7000,
            100,
            FAIL,
            NULL,
        },
        {   "read overlapping EOA/EOF fails (addr < (EOA==EOF) < (addr+len))",
            6464,
            6400,
            100,
            FAIL,
            NULL,
        },
        {   "read between EOF and EOA fails (EOF < addr < (addr+len) < EOA)",
            8000,
            7000,
            100,
            FAIL,
            NULL,
        },
    };
    unsigned          testcase_count   = 6;
    unsigned          test_i           = 0;
    struct testcase   test;
    herr_t            open_return      = FAIL;
    char              buffer[S3_TEST_MAX_URL_SIZE];
    unsigned int      i                = 0;
    H5FD_t           *file_raven       = NULL;
    hid_t             fapl_id          = -1;

    TESTING("ROS3 VFD read/range-gets");

    if (s3_test_credentials_loaded == 0) {
        SKIPPED();
        puts("    s3 credentials are not loaded");
        fflush(stdout);
        return 0;
    }

    if (FALSE == s3_test_bucket_defined) {
        SKIPPED();
        puts("    environment variable HDF5_ROS3_TEST_BUCKET_URL not defined");
        fflush(stdout);
        return 0;
    }

    /*********
     * SETUP *
     *********/

    /* create ROS3 fapl
     */
    fapl_id = H5Pcreate(H5P_FILE_ACCESS);
    FAIL_IF( fapl_id < 0 )
    FAIL_IF( FAIL == H5Pset_fapl_ros3(fapl_id, &restricted_access_fa) )

    /* open file
     */
    file_raven = H5FDopen( /* will open with "authenticating" fapl */
            url_text_public, /* TODO: check return state: anon access of restricted says OK? (not NULL) */
            H5F_ACC_RDONLY,
            fapl_id,
            HADDR_UNDEF); /* Demonstrate success with "automatic" value */
    FAIL_IF( NULL == file_raven )

    JSVERIFY( 6464, H5FDget_eof(file_raven, H5FD_MEM_DEFAULT), NULL )

    /*********
     * TESTS *
     *********/

    for (test_i = 0; test_i < testcase_count; test_i++) {

        /* -------------- *
         * per-test setup *
         * -------------- */

        test        = cases[test_i];
        open_return = FAIL;

        FAIL_IF( S3_TEST_MAX_URL_SIZE < test.len ) /* buffer too small! */

        FAIL_IF( FAIL ==
                 H5FD_set_eoa( file_raven, H5FD_MEM_DEFAULT, test.eoa_set) )

        for (i = 0; i < S3_TEST_MAX_URL_SIZE; i++) /* zero buffer contents */
            buffer[i] = 0;

        /* ------------ *
         * conduct test *
         * ------------ */

        H5E_BEGIN_TRY {
            open_return = H5FDread(
                    file_raven,
                    H5FD_MEM_DRAW,
                    H5P_DEFAULT,
                    test.addr,
                    test.len,
                    buffer);
        } H5E_END_TRY;

        JSVERIFY( test.success,
                  open_return,
                  test.message )
        if (open_return == SUCCEED)
            JSVERIFY_STR( test.expected, buffer, NULL )

    } /* for each testcase */

    /************
     * TEARDOWN *
     ************/

    FAIL_IF( FAIL == H5FDclose(file_raven) )
    file_raven = NULL;

    FAIL_IF( FAIL == H5Pclose(fapl_id) )
    fapl_id = -1;

    PASSED();
    return 0;

error:
    /***********
     * CLEANUP *
     ***********/

    if (file_raven)
        (void)H5FDclose(file_raven);
    if (fapl_id >= 0) {
        H5E_BEGIN_TRY {
           (void)H5Pclose(fapl_id);
        } H5E_END_TRY;
    }

    return 1;

} /* test_read */


/*---------------------------------------------------------------------------
 *
 * Function: test_noops_and_autofails()
 *
 * Purpose:
 *
 *     Demonstrate the unavailable and do-nothing routines unique to
 *     Read-Only VFD.
 *
 * Return:
 *
 *     PASSED : 0
 *     FAILED : 1
 *
 * Programmer: Jacob Smith
 *             2017-11-06
 *
 *---------------------------------------------------------------------------
 */
static int
test_noops_and_autofails(void)
{
    /*********************
     * test-local macros *
     *********************/

    /*************************
     * test-local structures *
     *************************/

    /************************
     * test-local variables *
     ************************/

    hbool_t           curl_ready = FALSE;
    hid_t             fapl_id    = -1;
    H5FD_t           *file       = NULL;
    const char        data[36]   = "The Force shall be with you, always";

    TESTING("ROS3 VFD always-fail and no-op routines");


    if (FALSE == s3_test_bucket_defined) {
        SKIPPED();
        puts("    environment variable HDF5_ROS3_TEST_BUCKET_URL not defined");
        fflush(stdout);
        return 0;
    }

    /*********
     * SETUP *
     *********/

    FAIL_IF( CURLE_OK != curl_global_init(CURL_GLOBAL_DEFAULT) )
    curl_ready = TRUE;

    /* create ROS3 fapl
     */
    fapl_id = H5Pcreate(H5P_FILE_ACCESS);
    FAIL_IF( fapl_id < 0 )
    JSVERIFY( SUCCEED, H5Pset_fapl_ros3(fapl_id, &anonymous_fa), NULL )

    /* open file
     */
    file = H5FDopen(
            url_text_public,
            H5F_ACC_RDONLY,
            fapl_id,
            HADDR_UNDEF);
    FAIL_IF( NULL == file )

    /*********
     * TESTS *
     *********/

    /* auto-fail calls to write and truncate
     */
    H5E_BEGIN_TRY {
        JSVERIFY( FAIL,
                  H5FDwrite(file, H5FD_MEM_DRAW, H5P_DEFAULT, 1000, 35, data),
                  "write must fail" )
    } H5E_END_TRY;

    H5E_BEGIN_TRY {
        JSVERIFY( FAIL,
                  H5FDtruncate(file, H5P_DEFAULT, FALSE),
                  "truncate must fail" )
    } H5E_END_TRY;

    H5E_BEGIN_TRY {
        JSVERIFY( FAIL,
                  H5FDtruncate(file, H5P_DEFAULT, TRUE),
                  "truncate must fail (closing)" )
    } H5E_END_TRY;

    /* no-op calls to `lock()` and `unlock()`
     */
    JSVERIFY( SUCCEED,
              H5FDlock(file, TRUE),
              "lock always succeeds; has no effect" )
    JSVERIFY( SUCCEED,
              H5FDlock(file, FALSE),
              NULL )
    JSVERIFY( SUCCEED,
              H5FDunlock(file),
              NULL )
    /* Lock/unlock with null file or similar error crashes tests.
     * HDassert in calling heirarchy, `H5FD[un]lock()` and `H5FD_[un]lock()`
     */

    /************
     * TEARDOWN *
     ************/

    FAIL_IF( FAIL == H5FDclose(file) )
    file = NULL;

    FAIL_IF( FAIL == H5Pclose(fapl_id) )
    fapl_id = -1;

    curl_global_cleanup();
    curl_ready = FALSE;

    PASSED();
    return 0;

error:
    /***********
     * CLEANUP *
     ***********/

    if (fapl_id >= 0) {
        H5E_BEGIN_TRY {
           (void)H5Pclose(fapl_id);
        } H5E_END_TRY;
    }
    if (file)               { (void)H5FDclose(file); }
    if (curl_ready == TRUE) { curl_global_cleanup(); }

    return 1;

} /* test_noops_and_autofails*/


/*---------------------------------------------------------------------------
 *
 * Function: test_cmp()
 *
 * Purpose:
 *
 *     Verify "file comparison" behavior.
 *
 * Return:
 *
 *     PASSED : 0
 *     FAILED : 1
 *
 * Programmer: Jacob Smith
 *             2017-11-06
 *
 *---------------------------------------------------------------------------
 */
static int
test_cmp(void)
{

    /*********************
     * test-local macros *
     *********************/

    /*************************
     * test-local structures *
     *************************/

    /************************
     * test-local variables *
     ************************/

    H5FD_t           *fd_raven   = NULL;
    H5FD_t           *fd_shakes  = NULL;
    H5FD_t           *fd_raven_2 = NULL;
    hbool_t           curl_ready = FALSE;
    hid_t             fapl_id    = -1;

    TESTING("ROS3 cmp (comparison)");

    if (s3_test_credentials_loaded == 0) {
        SKIPPED();
        puts("    s3 credentials are not loaded");
        fflush(stdout);
        return 0;
    }

    if (FALSE == s3_test_bucket_defined) {
        SKIPPED();
        puts("    environment variable HDF5_ROS3_TEST_BUCKET_URL not defined");
        fflush(stdout);
        return 0;
    }

    /*********
     * SETUP *
     *********/

    FAIL_IF( CURLE_OK != curl_global_init(CURL_GLOBAL_DEFAULT) )
    curl_ready = TRUE;

    fapl_id = H5Pcreate(H5P_FILE_ACCESS);
    FAIL_IF( 0 > fapl_id )
    JSVERIFY( SUCCEED, H5Pset_fapl_ros3(fapl_id, &restricted_access_fa), NULL )

    fd_raven = H5FDopen(
            url_text_public,
            H5F_ACC_RDONLY,
            fapl_id,
            HADDR_UNDEF);
    FAIL_IF( NULL == fd_raven )

    fd_shakes = H5FDopen(
            url_text_restricted,
            H5F_ACC_RDONLY,
            fapl_id,
            HADDR_UNDEF);
    FAIL_IF( NULL == fd_shakes )

    fd_raven_2 = H5FDopen(
            url_text_public,
            H5F_ACC_RDONLY,
            fapl_id,
            HADDR_UNDEF);
    FAIL_IF( NULL == fd_raven_2 )

    /*********
     * TESTS *
     *********/

    JSVERIFY(  0, H5FDcmp(fd_raven,  fd_raven_2), NULL )
    JSVERIFY( -1, H5FDcmp(fd_raven,  fd_shakes),  NULL )
    JSVERIFY( -1, H5FDcmp(fd_shakes, fd_raven_2), NULL )

    /************
     * TEARDOWN *
     ************/

    FAIL_IF( FAIL == H5FDclose(fd_raven) )
    fd_raven = NULL;
    FAIL_IF( FAIL == H5FDclose(fd_shakes) )
    fd_shakes = NULL;
    FAIL_IF( FAIL == H5FDclose(fd_raven_2) )
    fd_raven_2 = NULL;
    FAIL_IF( FAIL == H5Pclose(fapl_id) )
    fapl_id = -1;

    curl_global_cleanup();
    curl_ready = FALSE;

    PASSED();
    return 0;

error:
    /***********
     * CLEANUP *
     ***********/

    if (fd_raven   != NULL)  (void)H5FDclose(fd_raven);
    if (fd_raven_2 != NULL)  (void)H5FDclose(fd_raven_2);
    if (fd_shakes  != NULL)  (void)H5FDclose(fd_shakes);
    if (TRUE == curl_ready)  curl_global_cleanup();
    if (fapl_id >= 0) {
        H5E_BEGIN_TRY {
            (void)H5Pclose(fapl_id);
        } H5E_END_TRY;
    }

    return 1;

} /* test_cmp */


/*---------------------------------------------------------------------------
 *
 * Function: test_H5F_integration()
 *
 * Purpose:
 *
 *     Demonstrate S3 file-open through H5F API.
 *
 * Return:
 *
 *     PASSED : 0
 *     FAILED : 1
 *
 * Programmer: Jacob Smith
 *             2017-11-07
 *
 *---------------------------------------------------------------------------
 */
static int
test_H5F_integration(void)
{
    /*********************
     * test-local macros *
     *********************/

    /*************************
     * test-local structures *
     *************************/

    /************************
     * test-local variables *
     ************************/

    hid_t file    = -1;
    hid_t fapl_id = -1;

    TESTING("S3 file access through HD5F library (H5F API)");

    if (s3_test_credentials_loaded == 0) {
        SKIPPED();
        puts("    s3 credentials are not loaded");
        fflush(stdout);
        return 0;
    }

    if (FALSE == s3_test_bucket_defined) {
        SKIPPED();
        puts("    environment variable HDF5_ROS3_TEST_BUCKET_URL not defined");
        fflush(stdout);
        return 0;
    }

    /*********
     * SETUP *
     *********/

    fapl_id = H5Pcreate(H5P_FILE_ACCESS);
    FAIL_IF( 0 > fapl_id )
    FAIL_IF( FAIL == H5Pset_fapl_ros3(fapl_id, &restricted_access_fa) )

    /*********
     * TESTS *
     *********/

    /* Read-Write Open access is not allowed with this file driver.
     */
    H5E_BEGIN_TRY {
        FAIL_IF( 0 <= H5Fopen(
                      url_h5_public,
                      H5F_ACC_RDWR,
                      fapl_id) )
    } H5E_END_TRY;

    /* H5Fcreate() is not allowed with this file driver.
     */
    H5E_BEGIN_TRY {
        FAIL_IF( 0 <= H5Fcreate(
                      url_missing,
                      H5F_ACC_RDONLY,
                      H5P_DEFAULT,
                      fapl_id) )
    } H5E_END_TRY;

    /* Successful open.
     */
    file = H5Fopen(
            url_h5_public,
            H5F_ACC_RDONLY,
            fapl_id);
    FAIL_IF( file < 0 )

    /************
     * TEARDOWN *
     ************/

    FAIL_IF( FAIL == H5Fclose(file) )
    file = -1;

    FAIL_IF( FAIL == H5Pclose(fapl_id) )
    fapl_id = -1;

    PASSED();
    return 0;

error:
    /***********
     * CLEANUP *
     ***********/
HDprintf("\nerror!"); fflush(stdout);

    if (fapl_id >= 0) {
        H5E_BEGIN_TRY {
           (void)H5Pclose(fapl_id);
        } H5E_END_TRY;
    }
    if (file > 0)
        (void)H5Fclose(file);

    return 1;

} /* test_H5F_integration */

#endif /* H5_HAVE_ROS3_VFD */


/*-------------------------------------------------------------------------
 *
 * Function:    main
 *
 * Purpose:     Tests the basic features of Virtual File Drivers
 *
 * Return:      Success: 0
 *              Failure: 1
 *
 * Programmer:  Jacob Smith
 *              2017-10-23
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
#ifdef H5_HAVE_ROS3_VFD
    int nerrors = 0;
    const char *bucket_url_env = NULL;

#endif /* H5_HAVE_ROS3_VFD */

    HDprintf("Testing ros3 VFD functionality.\n");

#ifdef H5_HAVE_ROS3_VFD

    /************************
     * initialize test urls *
     ************************/

    bucket_url_env = HDgetenv("HDF5_ROS3_TEST_BUCKET_URL");
    if (bucket_url_env == NULL || bucket_url_env[0] == '\0') {
        HDprintf("WARNING: S3 bucket url is not defined in enviornment " \
                 "variable 'HDF5_ROS3_TEST_BUCKET_URL'!\n");
    } else {
        HDstrncpy(s3_test_bucket_url, bucket_url_env, S3_TEST_MAX_URL_SIZE);
        s3_test_bucket_defined = TRUE;
    }

    if (S3_TEST_MAX_URL_SIZE < HDsnprintf(
            url_text_restricted,
            (size_t)S3_TEST_MAX_URL_SIZE,
            "%s/%s",
            (const char *)s3_test_bucket_url,
            (const char *)S3_TEST_RESOURCE_TEXT_RESTRICTED))
    {
        HDprintf("* ros3 setup failed (text_restricted) ! *\n");
        return 1;
    }
    if (S3_TEST_MAX_URL_SIZE < HDsnprintf(
            url_text_public,
            (size_t)S3_TEST_MAX_URL_SIZE,
            "%s/%s",
            (const char *)s3_test_bucket_url,
            (const char *)S3_TEST_RESOURCE_TEXT_PUBLIC))
    {
        HDprintf("* ros3 setup failed (text_public) ! *\n");
        return 1;
    }
    if (S3_TEST_MAX_URL_SIZE < HDsnprintf(
            url_h5_public,
            (size_t)S3_TEST_MAX_URL_SIZE,
            "%s/%s",
            (const char *)s3_test_bucket_url,
            (const char *)S3_TEST_RESOURCE_H5_PUBLIC))
    {
        HDprintf("* ros3 setup failed (h5_public) ! *\n");
        return 1;
    }
    if (S3_TEST_MAX_URL_SIZE < HDsnprintf(
            url_missing,
            S3_TEST_MAX_URL_SIZE,
            "%s/%s",
            (const char *)s3_test_bucket_url,
            (const char *)S3_TEST_RESOURCE_MISSING))
    {
        HDprintf("* ros3 setup failed (missing) ! *\n");
        return 1;
    }

    /**************************************
     * load credentials and prepare fapls *
     **************************************/

    /* "clear" profile data strings */
    s3_test_aws_access_key_id[0]     = '\0';
    s3_test_aws_secret_access_key[0] = '\0';
    s3_test_aws_region[0]            = '\0';

    /* attempt to load test credentials
     * if unable, certain tests will be skipped
     */
    if (SUCCEED == H5FD_s3comms_load_aws_profile(
            S3_TEST_PROFILE_NAME,
            s3_test_aws_access_key_id,
            s3_test_aws_secret_access_key,
            s3_test_aws_region))
    {
        s3_test_credentials_loaded = 1;
        HDstrncpy(restricted_access_fa.aws_region,
                (const char *)s3_test_aws_region,
                H5FD_ROS3_MAX_REGION_LEN);
        HDstrncpy(restricted_access_fa.secret_id,
                (const char *)s3_test_aws_access_key_id,
                H5FD_ROS3_MAX_SECRET_ID_LEN);
        HDstrncpy(restricted_access_fa.secret_key,
                (const char *)s3_test_aws_secret_access_key,
                H5FD_ROS3_MAX_SECRET_KEY_LEN);
    }

    /******************
     * commence tests *
     ******************/

    h5_reset();

    nerrors += test_fapl_config_validation();
    nerrors += test_ros3_fapl();
    nerrors += test_vfd_open();
    nerrors += test_eof_eoa();
    nerrors += test_H5FDread_without_eoa_set_fails();
    nerrors += test_read();
    nerrors += test_noops_and_autofails();
    nerrors += test_cmp();
    nerrors += test_H5F_integration();

    if (nerrors > 0) {
        HDprintf("***** %d ros3 TEST%s FAILED! *****\n",
                 nerrors,
                 nerrors > 1 ? "S" : "");
        nerrors = 1;
    } else {
        HDprintf("All ros3 tests passed.\n");
    }
    return nerrors; /* 0 if no errors, 1 if any errors */

#else

    HDprintf("SKIPPED - read-only S3 VFD not built\n");
    return EXIT_SUCCESS;

#endif /* H5_HAVE_ROS3_VFD */

} /* main() */

