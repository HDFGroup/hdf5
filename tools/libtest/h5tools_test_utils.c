/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
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
 * Purpose: unit-test functionality of the routines in `tools/lib/h5tools_utils`
 *
 * Jacob Smith 2017-11-10
 */

#include "h5tools.h"
#include "h5tools_utils.h"
#include "h5test.h"

#define UTIL_TEST_DEBUG 0

/*****************************************************************************
 *
 * FILE-LOCAL TESTING MACROS
 *
 * Purpose:
 *
 *     1. Upon test failure, goto-jump to single-location teardown in test
 *        function. E.g., `error:` (consistency with HDF corpus) or
 *        `failed:` (reflects purpose).
 *            >>> using "error", in part because `H5E_BEGIN_TRY` expects it.
 *     2. Increase clarity and reduce overhead found with `TEST_ERROR`.
 *        e.g., "if(somefunction(arg, arg2) < 0) TEST_ERROR:"
 *        requires reading of entire line to know whether this if/call is
 *        part of the test setup, test operation, or a test unto itself.
 *     3. Provide testing macros with optional user-supplied failure message;
 *        if not supplied (NULL), generate comparison output in the spirit of
 *        test-driven development. E.g., "expected 5 but was -3"
 *        User messages clarify test's purpose in code, encouraging description
 *        without relying on comments.
 *     4. Configurable expected-actual order in generated comparison strings.
 *        Some prefer `VERIFY(expected, actual)`, others
 *        `VERIFY(actual, expected)`. Provide preprocessor ifdef switch
 *        to satisfy both parties, assuming one paradigm per test file.
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
 *         JSVERIFY()       - long-int equality check; prints reason/comparison
 *         JSVERIFY_NOT()   - long-int inequality check; prints
 *         JSVERIFY_STR()   - string equality check; prints
 *
 *****************************************************************************/

H5_GCC_CLANG_DIAG_OFF("format")

/*----------------------------------------------------------------------------
 *
 * ifdef flag: JSVERIFY_EXP_ACT
 *
 * JSVERIFY macros accept arguments as (EXPECTED, ACTUAL[, reason])
 * default, if this is undefined, is (ACTUAL, EXPECTED[, reason])
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
 *----------------------------------------------------------------------------
 */
#define JSFAILED_AT()                                                                                        \
    {                                                                                                        \
        printf("*FAILED* at %s:%d in %s()...\n", __FILE__, __LINE__, __func__);                              \
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
 *----------------------------------------------------------------------------
 */
#define FAIL_IF(condition)                                                                                   \
    if (condition) {                                                                                         \
        JSFAILED_AT()                                                                                        \
        goto error;                                                                                          \
    }

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
 *----------------------------------------------------------------------------
 */
#define JSERR_LONG(expected, actual, reason)                                                                 \
    {                                                                                                        \
        JSFAILED_AT()                                                                                        \
        if (reason != NULL) {                                                                                \
            printf("%s\n", (reason));                                                                        \
        }                                                                                                    \
        else {                                                                                               \
            printf("  ! Expected %ld\n  ! Actual   %ld\n", (long)(expected), (long)(actual));                \
        }                                                                                                    \
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
 *----------------------------------------------------------------------------
 */
#define JSERR_STR(expected, actual, reason)                                                                  \
    {                                                                                                        \
        JSFAILED_AT()                                                                                        \
        if ((reason) != NULL) {                                                                              \
            printf("%s\n", (reason));                                                                        \
        }                                                                                                    \
        else {                                                                                               \
            printf("!!! Expected:\n%s\n!!!Actual:\n%s\n", (expected), (actual));                             \
        }                                                                                                    \
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
 *----------------------------------------------------------------------------
 */
#define JSVERIFY(expected, actual, reason)                                                                   \
    if ((long)(actual) != (long)(expected)) {                                                                \
        JSERR_LONG((expected), (actual), (reason))                                                           \
        goto error;                                                                                          \
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
 *----------------------------------------------------------------------------
 */
#define JSVERIFY_NOT(expected, actual, reason)                                                               \
    if ((long)(actual) == (long)(expected)) {                                                                \
        JSERR_LONG((expected), (actual), (reason))                                                           \
        goto error;                                                                                          \
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
 *----------------------------------------------------------------------------
 */
#define JSVERIFY_STR(expected, actual, reason)                                                               \
    if (strcmp((actual), (expected)) != 0) {                                                                 \
        JSERR_STR((expected), (actual), (reason));                                                           \
        goto error;                                                                                          \
    } /* JSVERIFY_STR */

#else /* JSVERIFY_EXP_ACT not defined                                        */
/* Repeats macros above, but with actual/expected parameters reversed. */

/*----------------------------------------------------------------------------
 * Macro: JSVERIFY()
 * See: JSVERIFY documentation above.
 *----------------------------------------------------------------------------
 */
#define JSVERIFY(actual, expected, reason)                                                                   \
    if ((long)(actual) != (long)(expected)) {                                                                \
        JSERR_LONG((expected), (actual), (reason));                                                          \
        goto error;                                                                                          \
    } /* JSVERIFY */

/*----------------------------------------------------------------------------
 * Macro: JSVERIFY_NOT()
 * See: JSVERIFY_NOT documentation above.
 *----------------------------------------------------------------------------
 */
#define JSVERIFY_NOT(actual, expected, reason)                                                               \
    if ((long)(actual) == (long)(expected)) {                                                                \
        JSERR_LONG((expected), (actual), (reason))                                                           \
        goto error;                                                                                          \
    } /* JSVERIFY_NOT */

/*----------------------------------------------------------------------------
 * Macro: JSVERIFY_STR()
 * See: JSVERIFY_STR documentation above.
 *----------------------------------------------------------------------------
 */
#define JSVERIFY_STR(actual, expected, reason)                                                               \
    if (strcmp((actual), (expected)) != 0) {                                                                 \
        JSERR_STR((expected), (actual), (reason));                                                           \
        goto error;                                                                                          \
    } /* JSVERIFY_STR */

#endif /* ifdef/else JSVERIFY_EXP_ACT */

/* if > 0, be very verbose when performing tests */
#define H5TOOLS_UTILS_TEST_DEBUG 0

/******************/
/* TEST FUNCTIONS */
/******************/

/*----------------------------------------------------------------------------
 *
 * Function: test_parse_tuple()
 *
 * Purpose:
 *
 *     Provide unit tests and specification for the `parse_tuple()` function.
 *
 * Return:
 *
 *     0   Tests passed.
 *     1   Tests failed.
 *
 *----------------------------------------------------------------------------
 */
static unsigned
test_parse_tuple(void)
{
    /*************************
     * TEST-LOCAL STRUCTURES *
     *************************/

    struct testcase {
        const char *test_msg;     /* info about test case */
        const char *in_str;       /* input string */
        int         sep;          /* separator "character" */
        herr_t      exp_ret;      /* expected SUCCEED / FAIL */
        unsigned    exp_nelems;   /* expected number of elements */
                                  /* (no more than 7!)           */
        const char *exp_elems[7]; /* list of elements (no more than 7!) */
    };

    /******************
     * TEST VARIABLES *
     ******************/

    struct testcase cases[] = {
        {
            "bad start",
            "words(before)",
            ';',
            FAIL,
            0,
            {NULL},
        },
        {
            "tuple not closed",
            "(not ok",
            ',',
            FAIL,
            0,
            {NULL},
        },
        {
            "empty tuple",
            "()",
            '-',
            SUCCEED,
            1,
            {""},
        },
        {
            "no separator",
            "(stuff keeps on going)",
            ',',
            SUCCEED,
            1,
            {"stuff keeps on going"},
        },
        {
            "4-ple, escaped separator",
            "(elem0,elem1,el\\,em2,elem3)", /* "el\,em" */
            ',',
            SUCCEED,
            4,
            {"elem0", "elem1", "el,em2", "elem3"},
        },
        {
            "5-ple, escaped escaped separator",
            "(elem0,elem1,el\\\\,em2,elem3)",
            ',',
            SUCCEED,
            5,
            {"elem0", "elem1", "el\\", "em2", "elem3"},
        },
        {
            "escaped non-comma separator",
            "(5-2-7-2\\-6-2)",
            '-',
            SUCCEED,
            5,
            {"5", "2", "7", "2-6", "2"},
        },
        {
            "embedded close-paren",
            "(be;fo)re)",
            ';',
            SUCCEED,
            2,
            {"be", "fo)re"},
        },
        {
            "embedded non-escaping backslash",
            "(be;fo\\re)",
            ';',
            SUCCEED,
            2,
            {"be", "fo\\re"},
        },
        {
            "double close-paren at end",
            "(be;fore))",
            ';',
            SUCCEED,
            2,
            {"be", "fore)"},
        },
        {
            "empty elements",
            "(;a1;;a4;)",
            ';',
            SUCCEED,
            5,
            {"", "a1", "", "a4", ""},
        },
        {
            "nested tuples with different separators",
            "((4,e,a);(6,2,a))",
            ';',
            SUCCEED,
            2,
            {"(4,e,a)", "(6,2,a)"},
        },
        {
            "nested tuples with same separators",
            "((4,e,a),(6,2,a))",
            ',',
            SUCCEED,
            6,
            {"(4", "e", "a)", "(6", "2", "a)"},
        },
        {
            "real-world use case",
            "(us-east-2,AKIAIMC3D3XLYXLN5COA,ugs5aVVnLFCErO/8uW14iWE3K5AgXMpsMlWneO/+)",
            ',',
            SUCCEED,
            3,
            {"us-east-2", "AKIAIMC3D3XLYXLN5COA", "ugs5aVVnLFCErO/8uW14iWE3K5AgXMpsMlWneO/+"},
        }};
    struct testcase tc;
    unsigned        n_tests       = 14;
    unsigned        i             = 0;
    unsigned        count         = 0;
    unsigned        elem_i        = 0;
    char          **parsed        = NULL;
    char           *cpy           = NULL;
    herr_t          success       = true;
    bool            show_progress = false;

    TESTING("arbitrary-count tuple parsing");

#if H5TOOLS_UTILS_TEST_DEBUG > 0
    show_progress = true;
#endif /* H5TOOLS_UTILS_TEST_DEBUG */

    /*********
     * TESTS *
     *********/

    for (i = 0; i < n_tests; i++) {

        /* SETUP
         */
        assert(parsed == NULL);
        assert(cpy == NULL);
        tc = cases[i];
        if (show_progress == true) {
            printf("testing %d: %s...\n", i, tc.test_msg);
        }

        /* VERIFY
         */
        success = parse_tuple(tc.in_str, tc.sep, &cpy, &count, &parsed);

        JSVERIFY(tc.exp_ret, success, "function returned incorrect value")
        JSVERIFY(tc.exp_nelems, count, (char *)NULL)
        if (success == SUCCEED) {
            FAIL_IF(parsed == NULL)
            for (elem_i = 0; elem_i < count; elem_i++) {
                JSVERIFY_STR(tc.exp_elems[elem_i], parsed[elem_i], (char *)NULL)
            }
            /* TEARDOWN */
            assert(parsed != NULL);
            assert(cpy != NULL);
            free(parsed);
            parsed = NULL;
            free(cpy);
            cpy = NULL;
        }
        else {
            FAIL_IF(parsed != NULL)
        } /* if parse_tuple() == SUCCEED or no */

    } /* for each testcase */

    PASSED();
    return 0;

error:
    /***********
     * CLEANUP *
     ***********/

    if (parsed != NULL)
        free(parsed);
    if (cpy != NULL)
        free(cpy);

    return 1;

} /* test_parse_tuple */

/*----------------------------------------------------------------------------
 *
 * Function:   test_populate_ros3_fa()
 *
 * Purpose:    Verify behavior of `populate_ros3_fa()`
 *
 * Return:     0 if test passes
 *             1 if failure
 *
 *----------------------------------------------------------------------------
 */
static unsigned
test_populate_ros3_fa(void)
{
#ifdef H5_HAVE_ROS3_VFD
    /*************************
     * TEST-LOCAL STRUCTURES *
     *************************/

    /************************
     * TEST-LOCAL VARIABLES *
     ************************/

    bool show_progress = false;
    int  bad_version   = 0xf87a; /* arbitrarily wrong version number */
#endif                           /* H5_HAVE_ROS3_VFD */

    TESTING("programmatic ros3 fapl population");

#ifndef H5_HAVE_ROS3_VFD
    puts(" -SKIP-");
    puts("    Test is skipped unless HDF5 is configured and built with the Read-Only S3 VFD enabled.");
    fflush(stdout);
    return 0;
#else
#if H5TOOLS_UTILS_TEST_DEBUG > 0
    show_progress = true;
#endif /* H5TOOLS_UTILS_TEST_DEBUG */

    assert(bad_version != H5FD_CURR_ROS3_FAPL_T_VERSION);

    /*********
     * TESTS *
     *********/

    /* NULL fapl config pointer fails
     */
    {
        const char *values[] = {"x", "y", "z"};

        if (show_progress) {
            printf("NULL fapl pointer\n");
        }

        JSVERIFY(0, h5tools_populate_ros3_fapl(NULL, values), "fapl pointer cannot be null")
    }

    /* NULL values pointer yields default fapl
     */
    {
        H5FD_ros3_fapl_ext_t fa = {{bad_version, true, "u", "v", "w"}, "x"};

        if (show_progress) {
            printf("NULL values pointer\n");
        }

        JSVERIFY(1, h5tools_populate_ros3_fapl(&fa, NULL), "NULL values pointer yields \"default\" fapl")
        JSVERIFY(H5FD_CURR_ROS3_FAPL_T_VERSION, fa.fa.version, (char *)NULL)
        JSVERIFY(false, fa.fa.authenticate, (char *)NULL)
        JSVERIFY_STR("", fa.fa.aws_region, (char *)NULL)
        JSVERIFY_STR("", fa.fa.secret_id, (char *)NULL)
        JSVERIFY_STR("", fa.fa.secret_key, (char *)NULL)
        JSVERIFY_STR("", fa.token, (char *)NULL)
    }

    /* all-empty values
     * yields default fapl
     */
    {
        H5FD_ros3_fapl_ext_t fa       = {{bad_version, true, "u", "v", "w"}, "x"};
        const char          *values[] = {"", "", "", ""};

        if (show_progress) {
            printf("all empty values\n");
        }

        JSVERIFY(1, h5tools_populate_ros3_fapl(&fa, values), "empty values yields \"default\" fapl")
        JSVERIFY(H5FD_CURR_ROS3_FAPL_T_VERSION, fa.fa.version, (char *)NULL)
        JSVERIFY(false, fa.fa.authenticate, (char *)NULL)
        JSVERIFY_STR("", fa.fa.aws_region, (char *)NULL)
        JSVERIFY_STR("", fa.fa.secret_id, (char *)NULL)
        JSVERIFY_STR("", fa.fa.secret_key, (char *)NULL)
        JSVERIFY_STR("", fa.token, (char *)NULL)
    }

    /* successfully set fapl with values
     * excess value is ignored
     */
    {
        H5FD_ros3_fapl_ext_t fa       = {{bad_version, false, "a", "b", "c"}, "d"};
        const char          *values[] = {"x", "y", "z", "a", "b"};

        if (show_progress) {
            printf("successful full set\n");
        }

        JSVERIFY(1, h5tools_populate_ros3_fapl(&fa, values), "four values")
        JSVERIFY(H5FD_CURR_ROS3_FAPL_T_VERSION, fa.fa.version, (char *)NULL)
        JSVERIFY(true, fa.fa.authenticate, (char *)NULL)
        JSVERIFY_STR("x", fa.fa.aws_region, (char *)NULL)
        JSVERIFY_STR("y", fa.fa.secret_id, (char *)NULL)
        JSVERIFY_STR("z", fa.fa.secret_key, (char *)NULL)
        JSVERIFY_STR("a", fa.token, (char *)NULL)
    }

    /* NULL region
     * yields default fapl
     */
    {
        H5FD_ros3_fapl_ext_t fa       = {{bad_version, false, "a", "b", "c"}, "d"};
        const char          *values[] = {NULL, "y", "z", ""};

        if (show_progress) {
            printf("NULL region\n");
        }

        JSVERIFY(0, h5tools_populate_ros3_fapl(&fa, values), "could not fill fapl")
        JSVERIFY(H5FD_CURR_ROS3_FAPL_T_VERSION, fa.fa.version, (char *)NULL)
        JSVERIFY(false, fa.fa.authenticate, (char *)NULL)
        JSVERIFY_STR("", fa.fa.aws_region, (char *)NULL)
        JSVERIFY_STR("", fa.fa.secret_id, (char *)NULL)
        JSVERIFY_STR("", fa.fa.secret_key, (char *)NULL)
        JSVERIFY_STR("", fa.token, (char *)NULL)
    }

    /* empty region
     * yields default fapl
     */
    {
        H5FD_ros3_fapl_ext_t fa       = {{bad_version, false, "a", "b", "c"}, "d"};
        const char          *values[] = {"", "y", "z", ""};

        if (show_progress) {
            printf("empty region; non-empty id, key\n");
        }

        JSVERIFY(0, h5tools_populate_ros3_fapl(&fa, values), "could not fill fapl")
        JSVERIFY(H5FD_CURR_ROS3_FAPL_T_VERSION, fa.fa.version, (char *)NULL)
        JSVERIFY(false, fa.fa.authenticate, (char *)NULL)
        JSVERIFY_STR("", fa.fa.aws_region, (char *)NULL)
        JSVERIFY_STR("", fa.fa.secret_id, (char *)NULL)
        JSVERIFY_STR("", fa.fa.secret_key, (char *)NULL)
        JSVERIFY_STR("", fa.token, (char *)NULL)
    }

    /* region overflow
     * yields default fapl
     */
    {
        H5FD_ros3_fapl_ext_t fa       = {{bad_version, false, "a", "b", "c"}, "d"};
        const char          *values[] = {"somewhere over the rainbow not too high "
                                         "there is another rainbow bounding some darkened sky",
                                "y", "z", ""};

        if (show_progress) {
            printf("region overflow\n");
        }

        assert(strlen(values[0]) > H5FD_ROS3_MAX_REGION_LEN);

        JSVERIFY(0, h5tools_populate_ros3_fapl(&fa, values), "could not fill fapl")
        JSVERIFY(H5FD_CURR_ROS3_FAPL_T_VERSION, fa.fa.version, (char *)NULL)
        JSVERIFY(false, fa.fa.authenticate, (char *)NULL)
        JSVERIFY_STR("", fa.fa.aws_region, (char *)NULL)
        JSVERIFY_STR("", fa.fa.secret_id, (char *)NULL)
        JSVERIFY_STR("", fa.fa.secret_key, (char *)NULL)
        JSVERIFY_STR("", fa.token, (char *)NULL)
    }

    /* NULL id
     * yields default fapl
     */
    {
        H5FD_ros3_fapl_ext_t fa       = {{bad_version, false, "a", "b", "c"}, "d"};
        const char          *values[] = {"x", NULL, "z", ""};

        if (show_progress) {
            printf("NULL id\n");
        }

        JSVERIFY(0, h5tools_populate_ros3_fapl(&fa, values), "could not fill fapl")
        JSVERIFY(H5FD_CURR_ROS3_FAPL_T_VERSION, fa.fa.version, (char *)NULL)
        JSVERIFY(false, fa.fa.authenticate, (char *)NULL)
        JSVERIFY_STR("", fa.fa.aws_region, (char *)NULL)
        JSVERIFY_STR("", fa.fa.secret_id, (char *)NULL)
        JSVERIFY_STR("", fa.fa.secret_key, (char *)NULL)
        JSVERIFY_STR("", fa.token, (char *)NULL)
    }

    /* empty id (non-empty region, key)
     * yields default fapl
     */
    {
        H5FD_ros3_fapl_ext_t fa       = {{bad_version, false, "a", "b", "c"}, "d"};
        const char          *values[] = {"x", "", "z", ""};

        if (show_progress) {
            printf("empty id; non-empty region and key\n");
        }

        JSVERIFY(0, h5tools_populate_ros3_fapl(&fa, values), "could not fill fapl")
        JSVERIFY(H5FD_CURR_ROS3_FAPL_T_VERSION, fa.fa.version, (char *)NULL)
        JSVERIFY(false, fa.fa.authenticate, (char *)NULL)
        JSVERIFY_STR("", fa.fa.aws_region, (char *)NULL)
        JSVERIFY_STR("", fa.fa.secret_id, (char *)NULL)
        JSVERIFY_STR("", fa.fa.secret_key, (char *)NULL)
        JSVERIFY_STR("", fa.token, (char *)NULL)
    }

    /* id overflow
     * partial set: region
     */
    {
        H5FD_ros3_fapl_ext_t fa       = {{bad_version, false, "a", "b", "c"}, "d"};
        const char          *values[] = {"x",
                                "Why is it necessary to solve the problem? "
                                         "What benefits will you receive by solving the problem? "
                                         "What is the unknown? "
                                         "What is it you don't yet understand? "
                                         "What is the information you have? "
                                         "What isn't the problem? "
                                         "Is the information insufficient, redundant, or contradictory? "
                                         "Should you draw a diagram or figure of the problem? "
                                         "What are the boundaries of the problem? "
                                         "Can you separate the various parts of the problem?",
                                "z", ""};

        if (show_progress) {
            printf("id overflow\n");
        }

        assert(strlen(values[1]) > H5FD_ROS3_MAX_SECRET_ID_LEN);

        JSVERIFY(0, h5tools_populate_ros3_fapl(&fa, values), "could not fill fapl")
        JSVERIFY(H5FD_CURR_ROS3_FAPL_T_VERSION, fa.fa.version, (char *)NULL)
        JSVERIFY(false, fa.fa.authenticate, (char *)NULL)
        JSVERIFY_STR("x", fa.fa.aws_region, (char *)NULL)
        JSVERIFY_STR("", fa.fa.secret_id, (char *)NULL)
        JSVERIFY_STR("", fa.fa.secret_key, (char *)NULL)
        JSVERIFY_STR("", fa.token, (char *)NULL)
    }

    /* NULL key
     * yields default fapl
     */
    {
        H5FD_ros3_fapl_ext_t fa       = {{bad_version, false, "a", "b", "c"}, "d"};
        const char          *values[] = {"x", "y", NULL, ""};

        if (show_progress) {
            printf("NULL key\n");
        }

        JSVERIFY(0, h5tools_populate_ros3_fapl(&fa, values), "could not fill fapl")
        JSVERIFY(H5FD_CURR_ROS3_FAPL_T_VERSION, fa.fa.version, (char *)NULL)
        JSVERIFY(false, fa.fa.authenticate, (char *)NULL)
        JSVERIFY_STR("", fa.fa.aws_region, (char *)NULL)
        JSVERIFY_STR("", fa.fa.secret_id, (char *)NULL)
        JSVERIFY_STR("", fa.fa.secret_key, (char *)NULL)
        JSVERIFY_STR("", fa.token, (char *)NULL)
    }

    /* NULL token
     * yields default fapl
     */
    {
        H5FD_ros3_fapl_ext_t fa       = {{bad_version, false, "a", "b", "c"}, "d"};
        const char          *values[] = {"x", "y", "z", NULL};

        if (show_progress) {
            printf("NULL key\n");
        }

        JSVERIFY(0, h5tools_populate_ros3_fapl(&fa, values), "could not fill token")
        JSVERIFY(H5FD_CURR_ROS3_FAPL_T_VERSION, fa.fa.version, (char *)NULL)
        JSVERIFY(false, fa.fa.authenticate, (char *)NULL)
        JSVERIFY_STR("", fa.fa.aws_region, (char *)NULL)
        JSVERIFY_STR("", fa.fa.secret_id, (char *)NULL)
        JSVERIFY_STR("", fa.fa.secret_key, (char *)NULL)
        JSVERIFY_STR("", fa.token, (char *)NULL)
    }

    /* empty key (non-empty region, id)
     * yields authenticating fapl
     */
    {
        H5FD_ros3_fapl_ext_t fa       = {{bad_version, false, "a", "b", "c"}, "d"};
        const char          *values[] = {"x", "y", "", ""};

        if (show_progress) {
            printf("empty key; non-empty region and id\n");
        }

        JSVERIFY(1, h5tools_populate_ros3_fapl(&fa, values), "could not fill fapl")
        JSVERIFY(H5FD_CURR_ROS3_FAPL_T_VERSION, fa.fa.version, (char *)NULL)
        JSVERIFY(true, fa.fa.authenticate, (char *)NULL)
        JSVERIFY_STR("x", fa.fa.aws_region, (char *)NULL)
        JSVERIFY_STR("y", fa.fa.secret_id, (char *)NULL)
        JSVERIFY_STR("", fa.fa.secret_key, (char *)NULL)
        JSVERIFY_STR("", fa.token, (char *)NULL)
    }

    /* empty key, region (non-empty id)
     * yields default fapl
     */
    {
        H5FD_ros3_fapl_ext_t fa       = {{bad_version, false, "a", "b", "c"}, "d"};
        const char          *values[] = {"", "y", "", ""};

        if (show_progress) {
            printf("empty key and region; non-empty id\n");
        }

        JSVERIFY(0, h5tools_populate_ros3_fapl(&fa, values), "could not fill fapl")
        JSVERIFY(H5FD_CURR_ROS3_FAPL_T_VERSION, fa.fa.version, (char *)NULL)
        JSVERIFY(false, fa.fa.authenticate, (char *)NULL)
        JSVERIFY_STR("", fa.fa.aws_region, (char *)NULL)
        JSVERIFY_STR("", fa.fa.secret_id, (char *)NULL)
        JSVERIFY_STR("", fa.fa.secret_key, (char *)NULL)
        JSVERIFY_STR("", fa.token, (char *)NULL)
    }

    /* empty key, id (non-empty region)
     * yields default fapl
     */
    {
        H5FD_ros3_fapl_ext_t fa       = {{bad_version, false, "a", "b", "c"}, "d"};
        const char          *values[] = {"x", "", "", ""};

        if (show_progress) {
            printf("empty key and id; non-empty region\n");
        }

        JSVERIFY(0, h5tools_populate_ros3_fapl(&fa, values), "could not fill fapl")
        JSVERIFY(H5FD_CURR_ROS3_FAPL_T_VERSION, fa.fa.version, (char *)NULL)
        JSVERIFY(false, fa.fa.authenticate, (char *)NULL)
        JSVERIFY_STR("", fa.fa.aws_region, (char *)NULL)
        JSVERIFY_STR("", fa.fa.secret_id, (char *)NULL)
        JSVERIFY_STR("", fa.fa.secret_key, (char *)NULL)
        JSVERIFY_STR("", fa.token, (char *)NULL)
    }

    /* key overflow
     * partial set: region, id
     */
    {
        H5FD_ros3_fapl_ext_t fa       = {{bad_version, false, "a", "b", "c"}, "d"};
        const char          *values[] = {"x", "y",
                                "Why is it necessary to solve the problem? "
                                         "What benefits will you receive by solving the problem? "
                                         "What is the unknown? "
                                         "What is it you don't yet understand? "
                                         "What is the information you have? "
                                         "What isn't the problem? "
                                         "Is the information insufficient, redundant, or contradictory? "
                                         "Should you draw a diagram or figure of the problem? "
                                         "What are the boundaries of the problem? "
                                         "Can you separate the various parts of the problem?",
                                ""};

        if (show_progress) {
            printf("key overflow\n");
        }

        assert(strlen(values[2]) > H5FD_ROS3_MAX_SECRET_KEY_LEN);

        JSVERIFY(0, h5tools_populate_ros3_fapl(&fa, values), "could not fill fapl")
        JSVERIFY(H5FD_CURR_ROS3_FAPL_T_VERSION, fa.fa.version, (char *)NULL)
        JSVERIFY(false, fa.fa.authenticate, (char *)NULL)
        JSVERIFY_STR("x", fa.fa.aws_region, (char *)NULL)
        JSVERIFY_STR("y", fa.fa.secret_id, (char *)NULL)
        JSVERIFY_STR("", fa.fa.secret_key, (char *)NULL)
        JSVERIFY_STR("", fa.token, (char *)NULL)
    }

    /* use case
     */
    {
        H5FD_ros3_fapl_ext_t fa       = {{0, 0, "", "", ""}, ""};
        const char          *values[] = {"us-east-2", "AKIAIMC3D3XLYXLN5COA",
                                "ugs5aVVnLFCErO/8uW14iWE3K5AgXMpsMlWneO/+", ""};
        JSVERIFY(1, h5tools_populate_ros3_fapl(&fa, values), "unable to set use case")
        JSVERIFY(1, fa.fa.version, "version check")
        JSVERIFY(1, fa.fa.authenticate, "should authenticate")
    }

    PASSED();
    return 0;

error:
    /***********
     * CLEANUP *
     ***********/

    return 1;

#endif /* H5_HAVE_ROS3_VFD */

} /* test_populate_ros3_fa */

/*----------------------------------------------------------------------------
 *
 * Function:   test_set_configured_fapl()
 *
 * Purpose:    Verify `h5tools_get_fapl()` with ROS3 and HDFS VFDs
 *
 * Return:     0 if test passes
 *             1 if failure
 *
 *----------------------------------------------------------------------------
 */
static unsigned
test_set_configured_fapl(void)
{
#define UTIL_TEST_NOFAPL  1
#define UTIL_TEST_DEFAULT 2
#define UTIL_TEST_CREATE  3

    /*************************
     * TEST-LOCAL STRUCTURES *
     *************************/
    typedef struct testcase {
        const char message[88];
        int        expected;
        int        fapl_choice;
        const char vfdname[12];
        void      *conf_fa;
    } testcase;

    typedef struct other_fa_t {
        int a;
        int b;
        int c;
    } other_fa_t;

    /************************
     * TEST-LOCAL VARIABLES *
     ************************/

    hid_t      fapl_id  = H5I_INVALID_HID;
    other_fa_t wrong_fa = {0x432, 0xf82, 0x9093};
#ifdef H5_HAVE_ROS3_VFD
    H5FD_ros3_fapl_ext_t ros3_anon_fa = {{1, false, "", "", ""}, ""};
#endif /* H5_HAVE_ROS3_VFD */
#ifdef H5_HAVE_LIBHDFS
    H5FD_hdfs_fapl_t hdfs_fa = {
        1,    /* fapl version          */
        "",   /* namenode name         */
        0,    /* namenode port         */
        "",   /* kerberos ticket cache */
        "",   /* user name             */
        2048, /* stream buffer size    */
    };
#endif                    /* H5_HAVE_LIBHDFS */
    unsigned n_cases = 5; /* number of common testcases */
    testcase cases[] = {
        {
            "(common) H5P_DEFAULT with no struct should succeed",
            1,
            UTIL_TEST_DEFAULT,
            H5_DEFAULT_VFD_NAME,
            NULL,
        },
        {
            "(common) H5P_DEFAULT with (ignored) struct should succeed",
            1,
            UTIL_TEST_DEFAULT,
            H5_DEFAULT_VFD_NAME,
            &wrong_fa,
        },
        {
            "(common) provided fapl entry should not fail",
            1,
            UTIL_TEST_CREATE,
            H5_DEFAULT_VFD_NAME,
            NULL,
        },
        {
            "(common) provided fapl entry should not fail; ignores struct",
            1,
            UTIL_TEST_CREATE,
            H5_DEFAULT_VFD_NAME,
            &wrong_fa,
        },
        {
            "(common) should fail: unrecognized vfd name",
            0,
            UTIL_TEST_DEFAULT,
            "unknown",
            NULL,
        },

#ifdef H5_HAVE_ROS3_VFD
        /* WARNING: add number of ROS3 test cases after array definition
         */
        {
            "(ROS3) should fail: no fapl id, no struct",
            0,
            UTIL_TEST_NOFAPL,
            "ros3",
            NULL,
        },
        {
            "(ROS3) should fail: no struct",
            0,
            UTIL_TEST_CREATE,
            "ros3",
            NULL,
        },
        {
            "(ROS3) successful set",
            1,
            UTIL_TEST_CREATE,
            "ros3",
            &ros3_anon_fa,
        },
#endif /* H5_HAVE_ROS3_VFD */

#ifdef H5_HAVE_LIBHDFS
        /* WARNING: add number of HDFS test cases after array definition
         */
        {
            "(HDFS) should fail: no fapl id, no struct",
            0,
            UTIL_TEST_NOFAPL,
            "hdfs",
            NULL,
        },
        {
            "(HDFS) should fail: no struct",
            0,
            UTIL_TEST_CREATE,
            "hdfs",
            NULL,
        },
        {
            "(HDFS) successful set",
            1,
            UTIL_TEST_CREATE,
            "hdfs",
            &hdfs_fa,
        },
#endif /* H5_HAVE_LIBHDFS */

    }; /* testcases `cases` array */
    unsigned int i;

#ifdef H5_HAVE_ROS3_VFD
    n_cases += 3;
#endif /* H5_HAVE_ROS3_VFD */

#ifdef H5_HAVE_LIBHDFS
    n_cases += 3;
#endif /* H5_HAVE_LIBHDFS */

    TESTING("programmatic fapl set");

    for (i = 0; i < n_cases; i++) {
        h5tools_vfd_info_t vfd_info;
        hid_t              result;
        testcase           C = cases[i];

        fapl_id = H5I_INVALID_HID;

#if UTIL_TEST_DEBUG
        fprintf(stderr, "setup test %d\t%s\n", i, C.message);
        fflush(stderr);
#endif /* UTIL_TEST_DEBUG */

        /* per-test setup */
        if (C.fapl_choice == UTIL_TEST_DEFAULT) {
            fapl_id = H5P_DEFAULT;
        }
        else if (C.fapl_choice == UTIL_TEST_CREATE) {
            fapl_id = H5Pcreate(H5P_FILE_ACCESS);
            FAIL_IF(fapl_id < 0)
        }

#if UTIL_TEST_DEBUG
        fprintf(stderr, "before test\n");
        fflush(stderr);
#endif /* UTIL_TEST_DEBUG */

        /* test */
        vfd_info.type   = VFD_BY_NAME;
        vfd_info.info   = C.conf_fa;
        vfd_info.u.name = C.vfdname;

        if (C.expected == 1)
            result = h5tools_get_fapl(H5P_DEFAULT, NULL, &vfd_info);
        else {
            H5E_BEGIN_TRY
            {
                result = h5tools_get_fapl(H5P_DEFAULT, NULL, &vfd_info);
            }
            H5E_END_TRY
        }

        if (C.expected == 0) {
            JSVERIFY(result, H5I_INVALID_HID, C.message)
        }
        else {
            JSVERIFY_NOT(result, H5I_INVALID_HID, C.message)
        }

#if UTIL_TEST_DEBUG
        fprintf(stderr, "after test\n");
        fflush(stderr);
#endif /* UTIL_TEST_DEBUG */

        /* per-test-teardown */
        if (fapl_id > 0) {
            FAIL_IF(FAIL == H5Pclose(fapl_id))
        }
        fapl_id = H5I_INVALID_HID;

#if UTIL_TEST_DEBUG
        fprintf(stderr, "after cleanup\n");
        fflush(stderr);
#endif /* UTIL_TEST_DEBUG */
    }

#if UTIL_TEST_DEBUG
    fprintf(stderr, "after loop\n");
    fflush(stderr);
#endif /* UTIL_TEST_DEBUG */

    PASSED();
    return 0;

error:
    /***********
     * CLEANUP *
     ***********/

#if UTIL_TEST_DEBUG
    fprintf(stderr, "ERROR\n");
    fflush(stderr);
#endif /* UTIL_TEST_DEBUG */

    if (fapl_id > 0) {
        (void)H5Pclose(fapl_id);
    }

    return 1;

#undef UTIL_TEST_NOFAPL
#undef UTIL_TEST_DEFAULT
#undef UTIL_TEST_CREATE
} /* test_set_configured_fapl */
H5_GCC_CLANG_DIAG_ON("format")

/*----------------------------------------------------------------------------
 *
 * Function:   main()
 *
 * Purpose:    Run all test functions.
 *
 * Return:     0 iff all test pass
 *             1 iff any failures
 *
 *----------------------------------------------------------------------------
 */
int
main(void)
{
    unsigned nerrors = 0;

#ifdef _H5TEST_
    h5reset(); /* h5test? */
#endif         /* _H5TEST_ */

    fprintf(stdout, "Testing h5tools_utils corpus.\n");

    nerrors += test_parse_tuple();
    nerrors += test_populate_ros3_fa();
    nerrors += test_set_configured_fapl();

    if (nerrors > 0) {
        fprintf(stdout, "***** %d h5tools_utils TEST%s FAILED! *****\n", nerrors, nerrors > 1 ? "S" : "");
        nerrors = 1;
    }
    else {
        fprintf(stdout, "All h5tools_utils tests passed\n");
    }

    return (int)nerrors;

} /* main */
