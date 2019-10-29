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
 * Read-Only HDFS Virtual File Driver (VFD)
 *
 * Purpose:
 *
 *     Verify behavior for Read-Only HDFS VFD.
 *
 *     Demonstrates basic use cases and fapl interaction.
 *
 * Programmer: Jacob Smith <jake.smith@hdfgroup.org>
 *             2018-04-23
 */

#include "h5test.h"      /* testing utilities */
#include "H5FDhdfs.h"    /* this file driver's utilities */


#ifdef H5_HAVE_LIBHDFS
#define HDFS_TEST_DEBUG 0
#define HDFS_TEST_MAX_BUF_SIZE 256
#endif /* H5_HAVE_LIBHDFS */

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
#endif /* UNUSED */


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

/* copied from src/hdfs.c
 */
#ifdef H5_HAVE_LIBHDFS
#define MAXADDR (((haddr_t)1<<(8*sizeof(HDoff_t)-1))-1)
#endif /*  H5_HAVE_LIBHDFS */

#define HDFS_NAMENODE_NAME_MAX_SIZE 128

/*******************************
 * FILE-LOCAL GLOBAL VARIABLES *
 *******************************/

#ifdef H5_HAVE_LIBHDFS
static const char filename_missing[]    = "/tmp/missing.txt";
static const char filename_bard[]       = "/tmp/t8.shakespeare.txt";
static const char filename_raven[]      = "/tmp/Poe_Raven.txt";
static const char filename_example_h5[] = "/tmp/t.h5";
#endif /*  H5_HAVE_LIBHDFS */

static H5FD_hdfs_fapl_t default_fa      = {
    1,    /* fa version */
    "localhost",   /* namenode name */
    0,    /* namenode port */
    "",   /* user name */
    "",   /* kerberos path */
    1024, /* buffer size */
};

/******************
 * TEST FUNCTIONS *
 ******************/


/*---------------------------------------------------------------------------
 *
 * Function: test_fapl_config_validation()
 *
 * Purpose:
 *
 *     Test data consistency of fapl configuration.
 *     Tests `H5FD_hdfs_validate_config` indirectly through `H5Pset_fapl_hdfs`.
 *
 * Return:
 *
 *     PASSED : 0
 *     FAILED : 1
 *
 * Programmer:  Jacob Smith
 *              2018-04-25
 *
 * Changes:     None.
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
        H5FD_hdfs_fapl_t  config;
    };

    /************************
     * test-local variables *
     ************************/

    hid_t            fapl_id     = -1;   /* file access property list ID */
    H5FD_hdfs_fapl_t config;
    H5FD_hdfs_fapl_t fa_fetch;
    herr_t           success     = SUCCEED;
    unsigned int     i           = 0;
    unsigned int     ncases      = 6;    /* should equal number of cases */
    struct testcase *case_ptr    = NULL; /* dumb work-around for possible     */
                                         /* dynamic cases creation because    */
                                         /* of compiler warnings Wlarger-than */
    struct testcase  cases_arr[] = {
        {   "default config fapl",
            SUCCEED,
            {   1,           /* version */
                "localhost", /* namenode_name */
                0,           /* namenode_port number */
                "some_user", /* user_name */
                "",          /* kerberos_ticket_cache path */
                -1,          /* stream_buffer_size */
            },
        },
        {   "invalid version number (2)",
            FAIL,
            {   2,           /* version */
                "localhost", /* namenode_name */
                0,           /* namenode_port number */
                "some_user", /* user_name */
                "",          /* kerberos_ticket_cache path */
                -1,          /* stream_buffer_size */
            },
        },
        {   "invalid version number (0)",
            FAIL,
            {   0,           /* version */
                "localhost", /* namenode_name */
                0,           /* namenode_port number */
                "some_user", /* user_name */
                "",          /* kerberos_ticket_cache path */
                -1,          /* stream_buffer_size */
            },
        },
        {   "nonsense kerberos path still ok?",
            SUCCEED,
            {   1,           /* version */
                "localhost", /* namenode_name */
                0,           /* namenode_port number */
                "some_user", /* user_name */
                "pathToSomewhere", /* kerberos_ticket_cache path */
                -1,          /* stream_buffer_size */
            },
        },
        {   "namenode port number too high",
            FAIL,
            {   1,           /* version */
                "localhost", /* namenode_name */
                88000,       /* namenode_port number */
                "some_user", /* user_name */
                "",          /* kerberos_ticket_cache path */
                -1,          /* stream_buffer_size */
            },
        },
        {   "negative namenode port number",
            FAIL,
            {   1,           /* version */
                "localhost", /* namenode_name */
                -1,          /* namenode_port number */
                "some_user", /* user_name */
                "",          /* kerberos_ticket_cache path */
                -1,          /* stream_buffer_size */
            },
        },
    };

    TESTING("HDFS fapl configuration validation");

    /*********
     * TESTS *
     *********/

    for (i = 0; i < ncases; i++) {

        /*---------------
         * per-test setup
         *---------------
         */
        case_ptr = &cases_arr[i];
        fapl_id = H5Pcreate(H5P_FILE_ACCESS);
        FAIL_IF( fapl_id < 0 ) /* sanity-check */

        /*-----------------------------------
         * Actually test -- set fapl.
         * Mute stack trace in failure cases.
         *-----------------------------------
         */
        H5E_BEGIN_TRY {
            /* `H5FD_hdfs_validate_config(...)` is static/private
             * to src/hdfs.c and cannot (and should not?) be tested directly?
             * Instead, validate config through public api.
             */
            success = H5Pset_fapl_hdfs(fapl_id, &case_ptr->config);
        } H5E_END_TRY;

        JSVERIFY( case_ptr->expected, success, case_ptr->msg )

        /* Make sure we can get back what we put in.
         * Only valid if the fapl configuration does not result in error.
         */
        if (success == SUCCEED) {
            config = case_ptr->config;
            JSVERIFY( SUCCEED,
                      H5Pget_fapl_hdfs(fapl_id, &fa_fetch),
                      "unable to get fapl" )
            JSVERIFY( H5FD__CURR_HDFS_FAPL_T_VERSION,
                      fa_fetch.version,
                      "invalid version number" )
            JSVERIFY( config.version,
                      fa_fetch.version,
                      "version number mismatch" )
            JSVERIFY( config.namenode_port,
                      fa_fetch.namenode_port,
                      "namenode port mismatch" )
            JSVERIFY( config.stream_buffer_size,
                      fa_fetch.stream_buffer_size,
                      "streambuffer size mismatch" )
            JSVERIFY_STR( config.namenode_name,
                          fa_fetch.namenode_name,
                          NULL )
            JSVERIFY_STR( config.user_name,
                          fa_fetch.user_name,
                          NULL )
            JSVERIFY_STR( config.kerberos_ticket_cache,
                          fa_fetch.kerberos_ticket_cache,
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

} /* end test_fapl_config_validation() */


/*-------------------------------------------------------------------------
 *
 * Function:    test_hdfs_fapl()
 *
 * Purpose:     Tests the file handle interface for the HDFS driver.
 *
 *              For now, test only fapl & flags.  Extend as the
 *              work on the VFD continues.
 *
 * Return:      Success:        0
 *              Failure:        1
 *
 * Programmer:  Jacob Smith
 *              2018-04-25
 *
 * Changes:     None.
 *
 *-------------------------------------------------------------------------
 */
static int
test_hdfs_fapl(void)
{
    /************************
     * test-local variables *
     ************************/

    hid_t             fapl_id        = -1;  /* file access property list ID */
    hid_t             driver_id      = -1;  /* ID for this VFD              */
    unsigned long     driver_flags   =  0;  /* VFD feature flags            */
    H5FD_hdfs_fapl_t  hdfs_fa_0      = {
        1,    /* version*/
        "",   /* node name */
        9000, /* node port */
        "",   /* username */
        "",   /* kerb cache path */
        1024, /* stream buffer size */
    };

    TESTING("HDFS fapl ");

    /* Set property list and file name for HDFS driver.
     */
    fapl_id = H5Pcreate(H5P_FILE_ACCESS);
    FAIL_IF( fapl_id < 0 )

    FAIL_IF( FAIL == H5Pset_fapl_hdfs(fapl_id, &hdfs_fa_0) )

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

} /* end test_hdfs_fapl() */


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
 *             2018-06-07
 *
 *---------------------------------------------------------------------------
 */
static int
test_vfd_open(void)
{

#ifndef H5_HAVE_LIBHDFS
    TESTING("HDFS VFD-level open");
    SKIPPED();
    puts("    HDFS VFD is not enabled");
    fflush(stdout);
    return 0;

#else

    /*********************
     * test-local macros *
     *********************/

/* selectors for which fapl to use in testcase */
#define FAPL_H5P_DEFAULT  -2
#define FAPL_UNCONFIGURED -3 /* H5P_FILE_ACCESS */
#define FAPL_HDFS         -4

    /*************************
     * test-local structures *
     *************************/

    struct test_condition {
        const char *message;
        const char *url;
        unsigned    flags;
        int         which_fapl;
        haddr_t     maxaddr;
        hbool_t     might_use_other_driver;
    };

    /************************
     * test-local variables *
     ************************/

    struct test_condition failing_conditions[] = {
        {   "default property list (H5P_DEFAULT) is invalid",
            filename_bard,
            H5F_ACC_RDONLY,
            FAPL_H5P_DEFAULT,
            MAXADDR,
            TRUE,
        },
        {   "generic file access property list is invalid",
            filename_bard,
            H5F_ACC_RDONLY,
            FAPL_UNCONFIGURED,
            MAXADDR,
            TRUE,
        },
        {   "filename cannot be null",
            NULL,
            H5F_ACC_RDONLY,
            FAPL_HDFS,
            MAXADDR,
            FALSE,
        },
        {   "filename cannot be empty",
            "",
            H5F_ACC_RDONLY,
            FAPL_HDFS,
            MAXADDR,
            FALSE,
        },
        {   "file at filename must exist",
            filename_missing,
            H5F_ACC_RDONLY,
            FAPL_HDFS,
            MAXADDR,
            FALSE,
        },
        {   "read-write flag not supported",
            filename_bard,
            H5F_ACC_RDWR,
            FAPL_HDFS,
            MAXADDR,
            FALSE,
        },
        {   "truncate flag not supported",
            filename_bard,
            H5F_ACC_TRUNC,
            FAPL_HDFS,
            MAXADDR,
            FALSE,
        },
        {   "create flag not supported",
            filename_bard,
            H5F_ACC_CREAT,
            FAPL_HDFS,
            MAXADDR,
            FALSE,
        },
        {   "EXCL flag not supported",
            filename_bard,
            H5F_ACC_EXCL,
            FAPL_HDFS,
            MAXADDR,
            FALSE,
        },
        {   "maxaddr cannot be 0 (caught in `H5FD_open()`)",
            filename_bard,
            H5F_ACC_RDONLY,
            FAPL_HDFS,
            0,
            FALSE,
        },
    };
    unsigned  i                        = 0;
    unsigned  failing_conditions_count = 10;
    H5FD_t   *fd                       = NULL;
    hid_t     fapl_hdfs                = -1;
    hid_t     fapl_unconfigured        = -1;

    TESTING("HDFS VFD-level open");

    fapl_unconfigured = H5Pcreate(H5P_FILE_ACCESS);
    FAIL_IF( fapl_unconfigured < 0 )

    fapl_hdfs = H5Pcreate(H5P_FILE_ACCESS);
    FAIL_IF( fapl_hdfs < 0 )
    FAIL_IF( FAIL == H5Pset_fapl_hdfs(fapl_hdfs, &default_fa) )

    /*********
     * TESTS *
     *********/

    /* all the test cases that will _not_ open
     */
    for (i = 0; i < failing_conditions_count; i++) {
        struct test_condition T       = failing_conditions[i];
        hid_t                 fapl_id = H5P_DEFAULT;

        fd = NULL;

        if (T.which_fapl == FAPL_UNCONFIGURED) {
            fapl_id = fapl_unconfigured;
        }
        else
        if (T.which_fapl == FAPL_HDFS) {
            fapl_id = fapl_hdfs;
        }

#if HDFS_TEST_DEBUG
        HDfprintf(stderr, "testing: %s\n", T.message);
#endif /* HDFS_TEST_DEBUG */

        H5E_BEGIN_TRY {
            fd = H5FDopen(T.url, T.flags, fapl_id, T.maxaddr);
        } H5E_END_TRY;
        if (NULL != fd) {
            if (TRUE == T.might_use_other_driver &&
                H5FD_HDFS != fd->driver_id)
            {
                HDfprintf(stderr, "\n!!!!! WARNING !!!!!\n"              \
                          "    Successful open of file on local system " \
                          "with non-HDFS VFD.\n");
                JSVERIFY(SUCCEED, H5FDclose(fd),
                         "unable to close errant open");
                fd = NULL;
            }
            else {
                JSVERIFY(1, 0, T.message); /* print message and fail */
            }
        }
    }

    FAIL_IF( NULL != fd ) /* sanity check */

#if HDFS_TEST_DEBUG
        HDfprintf(stderr, "nominal open\n");
#endif /* HDFS_TEST_DEBUG */

    /* finally, show that a file can be opened
     */
    fd = H5FDopen(
            filename_bard,
            H5F_ACC_RDONLY,
            fapl_hdfs,
            MAXADDR);
    FAIL_IF( NULL == fd )

    /************
     * TEARDOWN *
     ************/

#if HDFS_TEST_DEBUG
        HDfprintf(stderr, "teardown...\n");
#endif /* HDFS_TEST_DEBUG */

    FAIL_IF( FAIL == H5FDclose(fd) )
    fd = NULL;

    FAIL_IF( FAIL == H5Pclose(fapl_hdfs) )
    fapl_hdfs = -1;

    FAIL_IF( FAIL == H5Pclose(fapl_unconfigured) )
    fapl_unconfigured = -1;

    PASSED();
    return 0;

error:

    /***********
     * CLEANUP *
     ***********/

    if (fd) {
        (void)H5FDclose(fd);
    }
    H5E_BEGIN_TRY {
        if (fapl_hdfs >= 0) {
            (void)H5Pclose(fapl_hdfs);
        }
        if (fapl_unconfigured >= 0) {
            (void)H5Pclose(fapl_unconfigured);
        }
    } H5E_END_TRY;

    return 1;

#undef FAPL_H5P_DEFAULT
#undef FAPL_UNCONFIGURED
#undef FAPL_HDFS

#endif /* H5_HAVE_LIBHDFS */

} /* end test_vfd_open() */


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
 *             2018-06-07
 *
 *---------------------------------------------------------------------------
 */
static int
test_eof_eoa(void)
{
#ifndef H5_HAVE_LIBHDFS
    TESTING("HDFS eof/eoa gets and sets");
    SKIPPED();
    puts("    HDFS VFD is not enabled");
    fflush(stdout);
    return 0;

#else

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
    hid_t    fapl_id         = -1;

    TESTING("HDFS eof/eoa gets and sets");

    /*********
     * SETUP *
     *********/

    fapl_id = H5Pcreate(H5P_FILE_ACCESS);
    FAIL_IF( 0 > fapl_id )
    FAIL_IF( FAIL == H5Pset_fapl_hdfs(fapl_id, &default_fa) )

    fd_shakespeare = H5FDopen(
             filename_bard,
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
    fd_shakespeare = NULL;

    FAIL_IF( FAIL == H5Pclose(fapl_id) )
    fapl_id = -1;

    PASSED();
    return 0;

error:

    /***********
     * CLEANUP *
     ***********/

    if (fd_shakespeare != NULL) {
        (void)H5FDclose(fd_shakespeare);
    }
    if (fapl_id >= 0) {
        H5E_BEGIN_TRY {
            (void)H5Pclose(fapl_id);
        } H5E_END_TRY;
    }

    return 1;

#endif /* H5_HAVE_LIBHDFS */

} /* end test_eof_eoa() */


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
 *             2018-06-08
 *
 *-----------------------------------------------------------------------------
 */
static int
test_H5FDread_without_eoa_set_fails(void)
{
#ifndef H5_HAVE_LIBHDFS
    TESTING("HDFS VFD read-eoa temporal coupling library limitation");
    SKIPPED();
    puts("    HDFS VFD is not enabled");
    fflush(stdout);
    return 0;

#else

    char          buffer[HDFS_TEST_MAX_BUF_SIZE];
    unsigned int  i                = 0;
    H5FD_t       *file_shakespeare = NULL;
    hid_t         fapl_id          = -1;

    TESTING("HDFS VFD read-eoa temporal coupling library limitation");

    /*********
     * SETUP *
     *********/

    /* create HDFS fapl
     */
    fapl_id = H5Pcreate(H5P_FILE_ACCESS);
    FAIL_IF( fapl_id < 0 )
    FAIL_IF( FAIL == H5Pset_fapl_hdfs(fapl_id, &default_fa) )

    file_shakespeare = H5FDopen(
            filename_bard,
            H5F_ACC_RDONLY,
            fapl_id,
            MAXADDR);
    FAIL_IF( NULL == file_shakespeare )

    JSVERIFY( 0, H5FDget_eoa(file_shakespeare, H5FD_MEM_DEFAULT),
              "EoA should remain unset by H5FDopen" )

    /* zero buffer contents */
    for (i = 0; i < HDFS_TEST_MAX_BUF_SIZE; i++) {
        buffer[i] = 0;
    }

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
    for (i = 0; i < HDFS_TEST_MAX_BUF_SIZE; i++) {
        JSVERIFY( 0, (unsigned)buffer[i], "buffer was modified by write!" )
    }

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

    if (file_shakespeare) {
        (void)H5FDclose(file_shakespeare);
    }
    if (fapl_id >= 0) {
        H5E_BEGIN_TRY {
           (void)H5Pclose(fapl_id);
        } H5E_END_TRY;
    }

    return 1;

#endif /* H5_HAVE_LIBHDFS */

} /* end test_H5FDread_without_eoa_set_fails() */



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
 *             2018-06-08
 *
 *---------------------------------------------------------------------------
 */
static int
test_read(void)
{
#ifndef H5_HAVE_LIBHDFS
    TESTING("HDFS VFD read/range-gets");
    SKIPPED();
    puts("    HDFS VFD is not enabled");
    fflush(stdout);
    return 0;

#else

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
    char              buffer[HDFS_TEST_MAX_BUF_SIZE];
    unsigned int      i                = 0;
    H5FD_t           *file_raven       = NULL;
    hid_t             fapl_id          = -1;

    TESTING("HDFS VFD read/range-gets");

    /*********
     * SETUP *
     *********/

    /* create HDFS fapl
     */
    fapl_id = H5Pcreate(H5P_FILE_ACCESS);
    FAIL_IF( fapl_id < 0 )
    FAIL_IF( FAIL == H5Pset_fapl_hdfs(fapl_id, &default_fa) )

    /* zero buffer contents */
    for (i = 0; i < HDFS_TEST_MAX_BUF_SIZE; i++) {
        buffer[i] = 0;
    }

    /* open file
     */
    file_raven = H5FDopen(
            filename_raven,
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

        FAIL_IF( HDFS_TEST_MAX_BUF_SIZE < test.len ) /* buffer too small! */

        FAIL_IF( FAIL ==
                 H5FDset_eoa( file_raven, H5FD_MEM_DEFAULT, test.eoa_set) )

        /* zero buffer contents */
        for (i = 0; i < HDFS_TEST_MAX_BUF_SIZE; i++) {
            buffer[i] = 0;
        }

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

        if (open_return == SUCCEED) {
            JSVERIFY_STR( test.expected, buffer, NULL )
        }

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

    if (file_raven != 0)  {
        (void)H5FDclose(file_raven);
    }
    if (fapl_id >= 0) {
        H5E_BEGIN_TRY {
           (void)H5Pclose(fapl_id);
        } H5E_END_TRY;
    }

    return 1;

#endif /* H5_HAVE_LIBHDFS */

} /* end test_read() */


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
 * Changes:
 *     + modify from S3VFD codebase to HDFS; Minor changes, mostly.
 *         + Jacob Smith 2018-06-08
 *
 *---------------------------------------------------------------------------
 */
static int
test_noops_and_autofails(void)
{
#ifndef H5_HAVE_LIBHDFS
    TESTING("HDFS VFD always-fail and no-op routines");
    SKIPPED();
    puts("    HDFS VFD is not enabled");
    fflush(stdout);
    return 0;

#else

    /*********************
     * test-local macros *
     *********************/

    /*************************
     * test-local structures *
     *************************/

    /************************
     * test-local variables *
     ************************/

    hid_t             fapl_id    = -1;
    H5FD_t           *file       = NULL;
    const char        data[36]   = "The Force shall be with you, always";

    TESTING("HDFS VFD always-fail and no-op routines");

    /*********
     * SETUP *
     *********/

    /* create HDFS fapl
     */
    fapl_id = H5Pcreate(H5P_FILE_ACCESS);
    FAIL_IF( fapl_id < 0 )
    FAIL_IF( FAIL == H5Pset_fapl_hdfs(fapl_id, &default_fa) )

    /* open file
     */
    file = H5FDopen(
            filename_bard,
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
    if (file != NULL) {
        (void)H5FDclose(file);
    }

    return 1;

#endif /* H5_HAVE_LIBHDFS */

} /* end test_noops_and_autofails() */


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
    TESTING("HDFS cmp (comparison)");
    SKIPPED();
    HDfprintf(
            stderr,
            "    TODO: Distinct valid fapls to open the same file.\n");

    return 0;

} /* end test_cmp() */


/*---------------------------------------------------------------------------
 *
 * Function: test_H5F_integration()
 *
 * Purpose:
 *
 *     Demonstrate H5F (File interface) behavior with files on HDFS.
 *
 * Return:
 *
 *     PASSED : 0
 *     FAILED : 1
 *
 * Programmer: Jacob Smith
 *             2017-11-07
 *
 * Changes:
 *     + modify from S3VFD codebase to HDFS; Minor changes, mostly.
 *         + Jacob Smith 2018-06-08
 *
 *---------------------------------------------------------------------------
 */
static int
test_H5F_integration(void)
{
#ifndef H5_HAVE_LIBHDFS
    TESTING("HDFS file access through HD5F library (H5F API)");
    SKIPPED();
    puts("    HDFS VFD is not enabled");
    fflush(stdout);
    return 0;

#else

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

    TESTING("HDFS file access through HD5F library (H5F API)");

    /*********
     * SETUP *
     *********/

    fapl_id = H5Pcreate(H5P_FILE_ACCESS);
    FAIL_IF( 0 > fapl_id )
    FAIL_IF( FAIL == H5Pset_fapl_hdfs(fapl_id, &default_fa) )

    /*********
     * TESTS *
     *********/

    /* Read-Write Open access is not allowed with this file driver.
     */
    H5E_BEGIN_TRY {
        FAIL_IF( 0 <= H5Fopen(
                      filename_example_h5,
                      H5F_ACC_RDWR,
                      fapl_id) )
    } H5E_END_TRY;

    /* H5Fcreate() is not allowed with this file driver.
     */
    H5E_BEGIN_TRY {
        FAIL_IF( 0 <= H5Fcreate(
                      filename_missing,
                      H5F_ACC_RDONLY,
                      H5P_DEFAULT,
                      fapl_id) )
    } H5E_END_TRY;

    /* Successful open.
     */
    file = H5Fopen(
            filename_example_h5,
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

#if HDFS_TEST_DEBUG
    HDprintf("\nerror!"); fflush(stdout);
#endif /* HDFS_TEST_DEBUG */

    if (fapl_id >= 0) {
        H5E_BEGIN_TRY {
           (void)H5Pclose(fapl_id);
        } H5E_END_TRY;
    }
    if (file > 0) {
        (void)H5Fclose(file);
    }

    return 1;

#endif /* H5_HAVE_LIBHDFS */

} /* test_H5F_integration */


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
    int nerrors = 0;

    /******************
     * commence tests *
     ******************/

    static char hdfs_namenode_name[HDFS_NAMENODE_NAME_MAX_SIZE] = "";
    const char *hdfs_namenode_name_env = NULL;

    hdfs_namenode_name_env = HDgetenv("HDFS_TEST_NAMENODE_NAME");
    if (hdfs_namenode_name_env == NULL || hdfs_namenode_name_env[0] == '\0') {
        HDstrncpy(hdfs_namenode_name, "localhost", HDFS_NAMENODE_NAME_MAX_SIZE);
    }
    else {
        HDstrncpy( /* TODO: error-check? */
                default_fa.namenode_name,
                hdfs_namenode_name_env,
                HDFS_NAMENODE_NAME_MAX_SIZE);
    }

    h5_reset();

    HDprintf("Testing hdfs VFD functionality.\n");

    nerrors += test_fapl_config_validation();
    nerrors += test_hdfs_fapl();
    nerrors += test_vfd_open();
    nerrors += test_eof_eoa();
    nerrors += test_H5FDread_without_eoa_set_fails();
    nerrors += test_read();
    nerrors += test_noops_and_autofails();
    nerrors += test_cmp();
    nerrors += test_H5F_integration();

    if (nerrors > 0) {
        HDprintf("***** %d hdfs TEST%s FAILED! *****\n",
                 nerrors,
                 nerrors > 1 ? "S" : "");
        nerrors = 1;
    }
    else {
        HDprintf("All hdfs tests passed.\n");
    }
    return nerrors; /* 0 if no errors, 1 if any errors */

} /* end main() */


