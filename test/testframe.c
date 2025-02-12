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
 * Purpose: Implements a basic testing framework for HDF5 tests to use.
 */

#include "testframe.h"
#include "h5test.h"

/*
 * Definitions for the testing structure.
 */

typedef struct TestStruct {
    char Name[MAXTESTNAME];
    char Description[MAXTESTDESC];
    void (*TestFunc)(void *);
    void (*TestSetupFunc)(void *);
    void (*TestCleanupFunc)(void *);
    void *TestParameters;
    int   TestNumErrors;
    int   TestSkipFlag;
} TestStruct;

/*
 * Global variables used by testing framework.
 */

static TestStruct *TestArray = NULL; /* Array of tests */
static unsigned    TestAlloc = 0;    /* Size of the Test array */
static unsigned    TestCount = 0;    /* Number of tests currently added to test array */

static const char *TestProgName                              = NULL;
static void (*TestPrivateUsage_g)(FILE *stream)              = NULL;
static herr_t (*TestPrivateParser_g)(int argc, char *argv[]) = NULL;
static herr_t (*TestCleanupFunc_g)(void)                     = NULL;

static int  TestNumErrs_g        = 0;    /* Total number of errors that occurred for whole test program */
static bool TestEnableErrorStack = true; /* Whether to show error stacks from the library */

static int TestMaxNumThreads_g = -1; /* Max number of threads that can be spawned */

static bool TestDoSummary_g = false; /* Show test summary. Default is no. */
static bool TestDoCleanUp_g = true;  /* Do cleanup or not. Default is yes. */

int TestFrameworkProcessID_g = 0;         /* MPI process rank value for parallel tests */
int TestVerbosity_g          = VERBO_DEF; /* Default Verbosity is Low */

/*
 * Add a new test to the list of tests to be executed
 */
herr_t
AddTest(const char *TestName, void (*TestFunc)(void *), void (*TestSetupFunc)(void *),
        void (*TestCleanupFunc)(void *), const void *TestData, size_t TestDataSize, const char *TestDescr)
{
    void *new_test_data = NULL;

    if (*TestName == '\0') {
        if (TestFrameworkProcessID_g == 0)
            fprintf(stderr, "%s: empty string given for test name\n", __func__);
        return FAIL;
    }
    if (strlen(TestName) >= MAXTESTNAME) {
        if (TestFrameworkProcessID_g == 0)
            fprintf(stderr, "%s: test name ('%s') too long, increase MAXTESTNAME(%d).\n", __func__, TestName,
                    MAXTESTNAME);
        return FAIL;
    }
    if (strlen(TestDescr) >= MAXTESTDESC) {
        if (TestFrameworkProcessID_g == 0)
            fprintf(stderr, "%s: test description ('%s') too long, increase MAXTESTDESC(%d).\n", __func__,
                    TestDescr, MAXTESTDESC);
        return FAIL;
    }
    if ((TestData && (0 == TestDataSize)) || (!TestData && (0 != TestDataSize))) {
        if (TestFrameworkProcessID_g == 0)
            fprintf(stderr, "%s: invalid test data size (%zu)\n", __func__, TestDataSize);
        return FAIL;
    }

    /* Re-allocate test array if necessary */
    if (TestCount >= TestAlloc) {
        TestStruct *newTest  = TestArray;
        unsigned    newAlloc = MAX(1, TestAlloc * 2);

        if (NULL == (newTest = realloc(TestArray, newAlloc * sizeof(TestStruct)))) {
            if (TestFrameworkProcessID_g == 0)
                fprintf(stderr,
                        "%s: couldn't reallocate test array, TestCount = %u, TestAlloc = %u, newAlloc = %u\n",
                        __func__, TestCount, TestAlloc, newAlloc);
            return FAIL;
        }

        TestArray = newTest;
        TestAlloc = newAlloc;
    }

    /* If the test name begins with '-', skip the test by default */
    if (*TestName == '-') {
        TestArray[TestCount].TestSkipFlag = 1;
        TestName++;
    }
    else
        TestArray[TestCount].TestSkipFlag = 0;

    strcpy(TestArray[TestCount].Name, TestName);
    strcpy(TestArray[TestCount].Description, TestDescr);

    /* Make a copy of the additional test data given */
    if (TestData) {
        if (NULL == (new_test_data = malloc(TestDataSize))) {
            if (TestFrameworkProcessID_g == 0)
                fprintf(stderr, "%s: couldn't allocate space for additional test data\n", __func__);
            return FAIL;
        }

        memcpy(new_test_data, TestData, TestDataSize);
    }
    TestArray[TestCount].TestParameters = new_test_data;

    TestArray[TestCount].TestFunc        = TestFunc;
    TestArray[TestCount].TestSetupFunc   = TestSetupFunc;
    TestArray[TestCount].TestCleanupFunc = TestCleanupFunc;
    TestArray[TestCount].TestNumErrors   = -1;

    TestCount++;

    return SUCCEED;
}

/*
 * Initialize the testing framework
 */
herr_t
TestInit(const char *ProgName, void (*TestPrivateUsage)(FILE *stream),
         herr_t (*TestPrivateParser)(int argc, char *argv[]), herr_t (*TestSetupFunc)(void),
         herr_t (*TestCleanupFunc)(void), int TestProcessID)
{
    /* Turn off automatic error reporting if requested */
    if (!TestEnableErrorStack) {
        if (H5Eset_auto2(H5E_DEFAULT, NULL, NULL) < 0) {
            if (TestFrameworkProcessID_g == 0)
                fprintf(stderr, "%s: can't disable error stack\n", __func__);
            return FAIL;
        }
    }

    /* Initialize value for TestExpress functionality */
    h5_get_testexpress();

    /* Record the program name and private routines if provided. */
    TestProgName = ProgName;
    if (NULL != TestPrivateUsage)
        TestPrivateUsage_g = TestPrivateUsage;
    if (NULL != TestPrivateParser)
        TestPrivateParser_g = TestPrivateParser;
    TestCleanupFunc_g = TestCleanupFunc;

    /* Set process ID for later use */
    TestFrameworkProcessID_g = TestProcessID;

    /* Set/reset global variables from h5test that may be used by
     * tests integrated with the testing framework
     */
    n_tests_run_g     = 0;
    n_tests_passed_g  = 0;
    n_tests_failed_g  = 0;
    n_tests_skipped_g = 0;

    /* Call test framework setup callback if provided */
    if (TestSetupFunc && TestSetupFunc() < 0) {
        if (TestFrameworkProcessID_g == 0)
            fprintf(stderr, "%s: error occurred in test framework initialization callback\n", __func__);
        return FAIL;
    }

    return SUCCEED;
}

/*
 * Print out test program usage help text
 */
void
TestUsage(FILE *stream)
{
    size_t max_test_name_len = 0;

    /* If running in parallel, only print output from a single MPI process */
    if (TestFrameworkProcessID_g != 0)
        return;

    if (!stream)
        stream = stdout;

    fprintf(stream, "Usage: %s [-v[erbose] (l[ow]|m[edium]|h[igh]|0-9)] %s\n", TestProgName,
            (TestPrivateUsage_g ? "<extra options>" : ""));
    fprintf(stream, "              [-[e]x[clude] name]+ \n");
    fprintf(stream, "              [-o[nly] name]+ \n");
    fprintf(stream, "              [-b[egin] name] \n");
    fprintf(stream, "              [-[max]t[hreads]]  \n");
    fprintf(stream, "              [-[testex]p[ress] (0-3)]  \n");
    fprintf(stream, "              [-s[ummary]]  \n");
    fprintf(stream, "              [-c[leanoff]]  \n");
    fprintf(stream, "              [-h[elp]]  \n");
    fprintf(stream, "\n\n");
    fprintf(stream, "verbose      controls the amount of information displayed\n");
    fprintf(stream, "exclude      to exclude tests by name\n");
    fprintf(stream, "only         to name tests which should be run\n");
    fprintf(stream, "begin        start at the name of the test given\n");
    fprintf(stream, "maxthreads   maximum number of threads to be used by multi-thread tests\n");
    fprintf(stream, "testexpress  set the TestExpress level for expediting tests\n");
    fprintf(stream, "               lower values run more tests; higher values skip more tests\n");
    fprintf(stream, "summary      prints a summary of test results at the end\n");
    fprintf(stream, "cleanoff     does not delete *.hdf files after execution of tests\n");
    fprintf(stream, "help         print out this information\n");
    if (TestPrivateUsage_g) {
        fprintf(stream, "\nExtra options\n");
        TestPrivateUsage_g(stream);
    }
    fprintf(stream, "\n\n");

    /* Collect some information for cleaner printing */
    for (unsigned Loop = 0; Loop < TestCount; Loop++) {
        size_t test_name_len = strlen(TestArray[Loop].Name);

        if (test_name_len > max_test_name_len)
            max_test_name_len = test_name_len;
    }

    fprintf(stream, "This program currently tests the following: \n\n");
    fprintf(stream, "%*s %s\n", (int)max_test_name_len, "Name", " Description");
    fprintf(stream, "%*s %s\n", (int)max_test_name_len, "----", " -----------");

    for (unsigned i = 0; i < TestCount; i++)
        fprintf(stream, "%*s  %s\n", (int)max_test_name_len, TestArray[i].Name, TestArray[i].Description);

    fprintf(stream, "\n\n");
}

/*
 * Print out miscellaneous test program information
 */
void
TestInfo(FILE *stream)
{
    unsigned major, minor, release;

    /* If running in parallel, only print output from a single MPI process */
    if (TestFrameworkProcessID_g != 0)
        return;

    if (!stream)
        stream = stdout;

    H5get_libversion(&major, &minor, &release);

    fprintf(stream, "\nFor help use: %s -help\n", TestProgName);
    fprintf(stream, "Linked with hdf5 version %u.%u release %u\n", major, minor, release);
}

/*
 * Parse command line information
 */
herr_t
TestParseCmdLine(int argc, char *argv[])
{
    herr_t ret_value = SUCCEED;

    while ((void)argv++, --argc > 0) {
        if ((strcmp(*argv, "-verbose") == 0) || (strcmp(*argv, "-v") == 0)) {
            if (argc > 0) {
                --argc;
                ++argv;

                if (ParseTestVerbosity(*argv) < 0) {
                    ret_value = FAIL;
                    goto done;
                }
            }
            else {
                ret_value = FAIL;
                goto done;
            }
        }
        else if (((strcmp(*argv, "-exclude") == 0) || (strcmp(*argv, "-x") == 0))) {
            if (argc > 0) {
                --argc;
                ++argv;

                if (SetTest(*argv, SKIPTEST) < 0) {
                    ret_value = FAIL;
                    goto done;
                }
            }
            else {
                ret_value = FAIL;
                goto done;
            }
        }
        else if (((strcmp(*argv, "-begin") == 0) || (strcmp(*argv, "-b") == 0))) {
            if (argc > 0) {
                --argc;
                ++argv;

                if (SetTest(*argv, BEGINTEST) < 0) {
                    ret_value = FAIL;
                    goto done;
                }
            }
            else {
                ret_value = FAIL;
                goto done;
            }
        }
        else if (((strcmp(*argv, "-only") == 0) || (strcmp(*argv, "-o") == 0))) {
            if (argc > 0) {
                --argc;
                ++argv;

                if (SetTest(*argv, ONLYTEST) < 0) {
                    ret_value = FAIL;
                    goto done;
                }
            }
            else {
                ret_value = FAIL;
                goto done;
            }
        }
        else if ((strcmp(*argv, "-summary") == 0) || (strcmp(*argv, "-s") == 0))
            TestDoSummary_g = true;
        else if (strcmp(*argv, "-disable-error-stack") == 0) {
            TestEnableErrorStack = false;
        }
        else if ((strcmp(*argv, "-help") == 0) || (strcmp(*argv, "-h") == 0)) {
            TestUsage(stdout);
            exit(EXIT_SUCCESS);
        }
        else if ((strcmp(*argv, "-cleanoff") == 0) || (strcmp(*argv, "-c") == 0))
            SetTestNoCleanup();
        else if ((strcmp(*argv, "-maxthreads") == 0) || (strcmp(*argv, "-t") == 0)) {
            if (argc > 0) {
                long max_threads;

                --argc;
                ++argv;

                errno       = 0;
                max_threads = strtol(*argv, NULL, 10);
                if (errno != 0) {
                    if (TestFrameworkProcessID_g == 0)
                        fprintf(stderr,
                                "error while parsing value (%s) specified for maximum number of threads\n",
                                *argv);
                    ret_value = FAIL;
                    goto done;
                }
                if (max_threads <= 0) {
                    if (TestFrameworkProcessID_g == 0)
                        fprintf(stderr, "invalid value (%ld) specified for maximum number of threads\n",
                                max_threads);
                    ret_value = FAIL;
                    goto done;
                }
                else if (max_threads > (long)INT_MAX) {
                    if (TestFrameworkProcessID_g == 0)
                        fprintf(stderr, "value (%ld) specified for maximum number of threads too large\n",
                                max_threads);
                    ret_value = FAIL;
                    goto done;
                }

                SetTestMaxNumThreads((int)max_threads);
            }
            else {
                ret_value = FAIL;
                goto done;
            }
        }
        else if ((strcmp(*argv, "-testexpress") == 0) || (strcmp(*argv, "-t") == 0)) {
            long test_express_level;

            if (argc <= 0 || !argv[1]) {
                if (TestFrameworkProcessID_g == 0)
                    fprintf(stderr, "no argument given to -testexpress option\n");
                ret_value = FAIL;
                goto done;
            }

            --argc;
            ++argv;

            errno              = 0;
            test_express_level = strtol(*argv, NULL, 10);
            if (errno != 0) {
                if (TestFrameworkProcessID_g == 0)
                    fprintf(stderr, "error while parsing value (%s) specified for TestExpress level\n",
                            *argv);
                ret_value = FAIL;
                goto done;
            }
            if (test_express_level < 0) {
                if (TestFrameworkProcessID_g == 0)
                    fprintf(stderr, "invalid value (%ld) specified for TestExpress level\n",
                            test_express_level);
                ret_value = FAIL;
                goto done;
            }

            /* Clamp value to current highest TestExpress level */
            if (test_express_level > H5_TEST_EXPRESS_SMOKE_TEST)
                test_express_level = H5_TEST_EXPRESS_SMOKE_TEST;

            SetTestExpress((int)test_express_level);
        }
        else {
            /* non-standard option.  Break out. */
            break;
        }
    }

    /* Call extra parsing function if provided. */
    if (NULL != TestPrivateParser_g) {
        if (TestPrivateParser_g(argc + 1, argv - 1) < 0) {
            ret_value = FAIL;
            goto done;
        }
    }

done:
    if (ret_value < 0)
        TestUsage(stderr);

    return ret_value;
}

/*
 * Execute all tests that aren't being skipped
 */
herr_t
PerformTests(void)
{
    /* Enable alarm timer for tests */
    if (TestAlarmOn() < 0)
        MESSAGE(5, ("Couldn't enable test alarm timer\n"));

    for (unsigned Loop = 0; Loop < TestCount; Loop++) {
        int old_num_errs = TestNumErrs_g;

        if (TestArray[Loop].TestSkipFlag) {
            MESSAGE(2, ("Skipping -- %s (%s) \n", TestArray[Loop].Description, TestArray[Loop].Name));
            continue;
        }

        MESSAGE(2, ("Testing  -- %s (%s) \n", TestArray[Loop].Description, TestArray[Loop].Name));
        MESSAGE(5, ("===============================================\n"));

        if (TestArray[Loop].TestSetupFunc)
            TestArray[Loop].TestSetupFunc(TestArray[Loop].TestParameters);

        TestArray[Loop].TestFunc(TestArray[Loop].TestParameters);

        if (TestArray[Loop].TestCleanupFunc)
            TestArray[Loop].TestCleanupFunc(TestArray[Loop].TestParameters);

        TestArray[Loop].TestNumErrors = TestNumErrs_g - old_num_errs;

        MESSAGE(5, ("===============================================\n"));
        MESSAGE(5, ("There were %d errors detected.\n\n", TestArray[Loop].TestNumErrors));
    }

    TestAlarmOff();

    MESSAGE(2, ("\n\n"));
    if (TestNumErrs_g)
        MESSAGE(VERBO_NONE, ("!!! %d Error(s) were detected !!!\n\n", TestNumErrs_g));
    else
        MESSAGE(VERBO_NONE, ("All tests were successful. \n\n"));

    return SUCCEED;
}

/*
 * Display a summary of running tests
 */
void
TestSummary(FILE *stream)
{
    size_t max_test_name_len    = 0;
    size_t max_test_desc_len    = 0;
    size_t test_name_header_len = 0;
    size_t test_desc_header_len = 0;

    /* If running in parallel, only print output from a single MPI process */
    if (TestFrameworkProcessID_g != 0)
        return;

    if (!stream)
        stream = stdout;

    /* Collect some information for cleaner printing */
    for (unsigned Loop = 0; Loop < TestCount; Loop++) {
        size_t test_name_len = strlen(TestArray[Loop].Name);
        size_t test_desc_len = strlen(TestArray[Loop].Description);

        if (test_name_len > max_test_name_len)
            max_test_name_len = test_name_len;
        if (test_desc_len > max_test_desc_len)
            max_test_desc_len = test_desc_len;
    }

    test_name_header_len = MAX(max_test_name_len, strlen("Name of Test"));
    test_desc_header_len = MAX(max_test_desc_len, strlen("Description of Test"));

    /* Print header, adjusted to maximum test name and description lengths */
    fprintf(stream, "Summary of Test Results:\n");
    fprintf(stream, "%-*s  Errors  %-*s\n", (int)test_name_header_len, "Name of Test",
            (int)test_desc_header_len, "Description of Test");

    /* Print a separating line row for each column header, adjusted to maximum
     * test name and description lengths
     */
    for (size_t i = 0; i < test_name_header_len; i++) /* 'Name of Test' */
        putc('-', stream);
    putc(' ', stream);
    putc(' ', stream);
    for (size_t i = 0; i < 6; i++) /* 'Errors' */
        putc('-', stream);
    putc(' ', stream);
    putc(' ', stream);
    for (size_t i = 0; i < test_desc_header_len; i++) /* 'Description of Test' */
        putc('-', stream);
    putc('\n', stream);

    for (unsigned Loop = 0; Loop < TestCount; Loop++) {
        if (TestArray[Loop].TestNumErrors == -1)
            fprintf(stream, "%-*s  %-6s  %-*s\n", (int)test_name_header_len, TestArray[Loop].Name, "N/A",
                    (int)test_desc_header_len, TestArray[Loop].Description);
        else
            fprintf(stream, "%-*s  %-6d  %-*s\n", (int)test_name_header_len, TestArray[Loop].Name,
                    TestArray[Loop].TestNumErrors, (int)test_desc_header_len, TestArray[Loop].Description);
    }

    fprintf(stream, "\n\n");
}

/*
 * Shutdown the test infrastructure
 */
herr_t
TestShutdown(void)
{
    /* Clean up test state first before tearing down testing framework */
    if (TestCleanupFunc_g && TestCleanupFunc_g() < 0) {
        if (TestFrameworkProcessID_g == 0)
            fprintf(stderr, "%s: error occurred in test framework initialization callback\n", __func__);
        return FAIL;
    }

    if (TestArray)
        for (unsigned Loop = 0; Loop < TestCount; Loop++)
            free(TestArray[Loop].TestParameters);

    free(TestArray);

    return SUCCEED;
}

/*
 * Retrieve the MPI rank for this process.
 */
H5_ATTR_PURE int
GetTestFrameworkProcessID(void)
{
    return TestFrameworkProcessID_g;
}

/*
 * Retrieve the verbosity level for the testing framework
 */
H5_ATTR_PURE int
GetTestVerbosity(void)
{
    return TestVerbosity_g;
}

/*
 * Set the verbosity level for the testing framework
 */
int
SetTestVerbosity(int newval)
{
    int oldval;

    if (newval < 0)
        newval = VERBO_NONE;
    else if (newval > VERBO_HI)
        newval = VERBO_HI;

    oldval          = TestVerbosity_g;
    TestVerbosity_g = newval;

    return oldval;
}

/*
 * Retrieve the TestExpress mode for the testing framework
 */
int
GetTestExpress(void)
{
    return h5_get_testexpress();
}

/*
 * Set the TestExpress mode for the testing framework.
 */
void
SetTestExpress(int newval)
{
    h5_set_testexpress(newval);
}

/*
 * Retrieve test summary request value.
 */
H5_ATTR_PURE bool
GetTestSummary(void)
{
    return TestDoSummary_g;
}

/*
 * Retrieve test file cleanup status value
 */
H5_ATTR_PURE bool
GetTestCleanup(void)
{
    /* Don't cleanup files if the HDF5_NOCLEANUP environment
     * variable is defined to anything
     */
    if (getenv(HDF5_NOCLEANUP))
        SetTestNoCleanup();

    return TestDoCleanUp_g;
}

/*
 * Set test file cleanup status to "don't clean up temporary files"
 */
void
SetTestNoCleanup(void)
{
    TestDoCleanUp_g = false;
}

/*
 * Parse an argument string for verbosity level and set it.
 */
herr_t
ParseTestVerbosity(char *argv)
{
    if (*argv == 'l')
        SetTestVerbosity(VERBO_LO);
    else if (*argv == 'm')
        SetTestVerbosity(VERBO_MED);
    else if (*argv == 'h')
        SetTestVerbosity(VERBO_HI);
    else {
        long verb_level;

        errno      = 0;
        verb_level = strtol(argv, NULL, 10);
        if (errno != 0) {
            if (TestFrameworkProcessID_g == 0)
                fprintf(stderr, "%s: error while parsing value (%s) specified for test verbosity\n", __func__,
                        argv);
            return FAIL;
        }

        if (verb_level < 0)
            verb_level = VERBO_DEF;
        else if (verb_level > VERBO_HI)
            verb_level = VERBO_HI;

        SetTestVerbosity((int)verb_level);
    }

    return SUCCEED;
}

/*
 * Retrieve the number of testing errors for the testing framework
 */
H5_ATTR_PURE int
GetTestNumErrs(void)
{
    return TestNumErrs_g;
}

/*
 * Increment the number of testing errors
 */
void
IncTestNumErrs(void)
{
    TestNumErrs_g++;
}

/*
 * This routine is designed to provide equivalent functionality to 'printf'
 * and also increment the error count for the testing framework.
 */
int
TestErrPrintf(const char *format, ...)
{
    va_list arglist;
    int     ret_value;

    /* Increment the error count */
    IncTestNumErrs();

    /* Print the requested information */
    va_start(arglist, format);
    ret_value = vfprintf(stderr, format, arglist);
    va_end(arglist);

    /* Return the length of the string produced (like printf() does) */
    return ret_value;
}

/*
 * Change testing behavior in relation to a specific test
 */
herr_t
SetTest(const char *testname, int action)
{
    static bool skipped_all = false;

    switch (action) {
        case SKIPTEST:
            for (unsigned Loop = 0; Loop < TestCount; Loop++)
                if (strcmp(testname, TestArray[Loop].Name) == 0) {
                    TestArray[Loop].TestSkipFlag = 1;
                    break;
                }
            break;
        case BEGINTEST:
            for (unsigned Loop = 0; Loop < TestCount; Loop++) {
                if (strcmp(testname, TestArray[Loop].Name) != 0)
                    TestArray[Loop].TestSkipFlag = 1;
                else {
                    /* Found it. Set it to run.  Done. */
                    TestArray[Loop].TestSkipFlag = 0;
                    break;
                }
            }
            break;
        case ONLYTEST:
            /* Skip all tests, then keep track that we did that.
             * Some testing prefers the convenience of being
             * able to specify multiple tests to "only" run
             * rather than specifying (possibly many more) tests
             * to exclude, but we only want to skip all the
             * tests a single time to facilitate this.
             */
            if (!skipped_all) {
                for (unsigned Loop = 0; Loop < TestCount; Loop++)
                    TestArray[Loop].TestSkipFlag = 1;
                skipped_all = true;
            }

            for (unsigned Loop = 0; Loop < TestCount; Loop++) {
                if (strcmp(testname, TestArray[Loop].Name) == 0) {
                    /* Found it. Set it to run. Break to skip the rest. */
                    TestArray[Loop].TestSkipFlag = 0;
                    break;
                }
            }
            break;
        default:
            /* error */
            if (TestFrameworkProcessID_g == 0)
                fprintf(stderr, "%s: invalid action %d specified\n", __func__, action);
            return FAIL;
    }

    return SUCCEED;
}

/*
 * Returns the value set for the maximum number of threads that a test
 * program can spawn in addition to the main thread.
 */
H5_ATTR_PURE int
GetTestMaxNumThreads(void)
{
    return TestMaxNumThreads_g;
}

/*
 * Set the value for the maximum number of threads that a test program
 * can spawn in addition to the main thread.
 */
herr_t
SetTestMaxNumThreads(int max_num_threads)
{
    TestMaxNumThreads_g = max_num_threads;

    return SUCCEED;
}

/* Enable a test timer that will kill long-running tests, the time is configurable
 * via an environment variable.
 *
 * Only useful on POSIX systems where alarm(2) is present. This does not include
 * MinGW builds, which will often incorrectly decide that alarm(2) exists.
 */
herr_t
TestAlarmOn(void)
{
    /* A TestExpress setting of H5_TEST_EXPRESS_EXHAUSTIVE should allow
     * tests to run for as long as necessary, so avoid enabling an
     * alarm-style timer here that would, by default, kill the test.
     */
    if (GetTestExpress() == H5_TEST_EXPRESS_EXHAUSTIVE)
        return SUCCEED;
#ifdef H5_HAVE_ALARM
    else {
        char         *env_val   = getenv("HDF5_ALARM_SECONDS"); /* Alarm environment */
        unsigned long alarm_sec = H5_ALARM_SEC;                 /* Number of seconds before alarm goes off */

        /* Get the alarm value from the environment variable, if set */
        if (env_val != NULL) {
            errno     = 0;
            alarm_sec = strtoul(env_val, NULL, 10);
            if (errno != 0) {
                if (TestFrameworkProcessID_g == 0)
                    fprintf(stderr, "%s: error while parsing value (%s) specified for alarm timeout\n",
                            __func__, env_val);
                return FAIL;
            }
            else if (alarm_sec > (unsigned long)UINT_MAX) {
                if (TestFrameworkProcessID_g == 0)
                    fprintf(stderr, "%s: value (%lu) specified for alarm timeout too large\n", __func__,
                            alarm_sec);
                return FAIL;
            }
        }

        /* Set the number of seconds before alarm goes off */
        alarm((unsigned)alarm_sec);
    }
#endif

    return SUCCEED;
}

/* Disable the test timer */
void
TestAlarmOff(void)
{
#ifdef H5_HAVE_ALARM
    /* Set the number of seconds to zero */
    alarm(0);
#endif
}
