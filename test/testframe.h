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
 * Purpose: Header file for a basic HDF5 testing framework
 */

#ifndef H5TESTFRAME_H
#define H5TESTFRAME_H

/*
 * Include generic testing header, which includes the public
 * HDF5 header, first. Including a private header first can
 * cause the library to #undef the H5OPEN macro and cause odd
 * test failures due to global IDs not being initialized.
 */
#include "h5test.h"

#include "H5private.h"

/**********/
/* Macros */
/**********/

/**
 * \def MAXTESTNAME
 * The maximum length for the name given to a test, including the NUL terminator
 */
#define MAXTESTNAME 64

/**
 * \def MAXTESTDESC
 * The maximum length for the description given to a test, including the NUL terminator
 */
#define MAXTESTDESC 128

/**
 * \def H5_ALARM_SEC
 * Number of seconds to wait before killing a test (requires alarm(2))
 */
#define H5_ALARM_SEC 1200 /* default is 20 minutes */

/*
 * Test controls definitions.
 */
#define SKIPTEST  1 /* Skip this test */
#define ONLYTEST  2 /* Do only this test */
#define BEGINTEST 3 /* Skip all tests before this test */

/*
 * Predefined test verbosity levels.
 *
 * Convention:
 *
 * The higher the verbosity value, the more information printed.
 * So, output for higher verbosity also include output of all lower
 * verbosity.
 */
//! <!-- [test_verbo_snip] -->
/*
 *
 *  Value     Description
 *  0         None:   No informational message.
 *  1                 "All tests passed"
 *  2                 Header of overall test
 *  3         Default: header and results of individual test
 *  4
 *  5         Low:    Major category of tests.
 *  6
 *  7         Medium: Minor category of tests such as functions called.
 *  8
 *  9         High:   Highest level.  All information.
 */
//! <!-- [test_verbo_snip] -->
#define VERBO_NONE 0 /* None    */
#define VERBO_DEF  3 /* Default */
#define VERBO_LO   5 /* Low     */
#define VERBO_MED  7 /* Medium  */
#define VERBO_HI   9 /* High    */

/*
 * Verbose queries
 * Only None needs an exact match.  The rest are at least as much.
 */
#define VERBOSE_NONE (GetTestVerbosity() == VERBO_NONE)
#define VERBOSE_DEF  (GetTestVerbosity() >= VERBO_DEF)
#define VERBOSE_LO   (GetTestVerbosity() >= VERBO_LO)
#define VERBOSE_MED  (GetTestVerbosity() >= VERBO_MED)
#define VERBOSE_HI   (GetTestVerbosity() >= VERBO_HI)

/* Used to document process through a test */
#define MESSAGE(V, A)                                                                                        \
    do {                                                                                                     \
        if (GetTestFrameworkProcessID() == 0 && GetTestVerbosity() > (V))                                    \
            printf A;                                                                                        \
    } while (0)

/************/
/* Typedefs */
/************/

/*************/
/* Variables */
/*************/

/**************/
/* Prototypes */
/**************/

#ifdef __cplusplus
extern "C" {
#endif

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Initializes the testing framework
 *
 * \param[in]  ProgName          The chosen name for the test executable to
 *                               be used
 * \param[in]  TestPrivateUsage  Pointer to a function which prints out
 *                               additional usage help text that is specific
 *                               to the test program
 * \param[in]  TestPrivateParser Pointer to a function which parses
 *                               command-line arguments which are specific to
 *                               the test program
 * \param[in]  TestSetupFunc     Pointer to a function which will be called
 *                               as part of TestInit()
 * \param[in]  TestCleanupFunc   Pointer to a function which will be called
 *                               when the testing framework is being shut
 *                               down
 * \param[in]  TestProcessID     ID for the process calling TestInit(). Used
 *                               to control printing of output in parallel
 *                               test programs.
 *
 * \return \herr_t
 *
 * \details TestInit() initializes the testing framework by setting up all
 *          the internal state needed for running tests. TestInit() should be
 *          called before any other function from this testing framework is
 *          called, but after other optional library setup functions, such
 *          as H5open() or H5dont_atexit().
 *
 *          \p ProgName is used to give a different name to the test program
 *          than the actual name of the executable. `argv[0]` should be
 *          passed for \p ProgName if a different name is not desired.
 *
 *          \p TestPrivateUsage is a pointer to a function that can be used
 *          to print out additional usage help text that is specific to the
 *          test program when necessary. The TestUsage() function calls this
 *          function to print out the additional help text after printing out
 *          a more general set of help test instructions. \p TestPrivateUsage
 *          may be NULL.
 *
 *          \p TestPrivateParser is a pointer to a function that can be used
 *          to parse command-line arguments which are specific to the test
 *          program. The TestParseCmdLine() function defers to this function
 *          when it encounters a command-line argument that is not among the
 *          standard list of arguments it recognizes. \p TestPrivateParser
 *          may be NULL.
 *
 *          \p TestSetupFunc is a pointer to a function that can be used to
 *          setup any state needed before tests begin executing. If provided,
 *          this callback function will be called as part of TestInit() once
 *          the testing framework has been fully initialized. \p TestSetupFunc
 *          may be NULL.
 *
 *          \p TestCleanupFunc is a pointer to a function that can be used
 *          to clean up any state after tests have finished executing. If
 *          provided, this callback function will be called by TestShutdown()
 *          before the testing framework starts being shut down.
 *          \p TestCleanupFunc may be NULL.
 *
 *          \p TestProcessID is an integer value that is used to distinguish
 *          between processes when multiple are involved in running a test
 *          program. This is primarily useful for controlling testing
 *          framework output printed during execution of a parallel test
 *          program. For serial tests, the value 0 should always be passed.
 *          For parallel tests, the rank value of the MPI process, as obtained
 *          by calling MPI_Comm_rank(), should be passed. Test framework output
 *          is only printed from the process with ID 0.
 *
 * \see TestShutdown(), TestUsage(), TestParseCmdLine()
 *
 */
H5TEST_DLL herr_t TestInit(const char *ProgName, void (*TestPrivateUsage)(FILE *stream),
                           int (*TestPrivateParser)(int argc, char *argv[]), herr_t (*TestSetupFunc)(void),
                           herr_t (*TestCleanupFunc)(void), int TestProcessID);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Shuts down the testing framework
 *
 * \return \herr_t
 *
 * \details TestShutdown() shuts down the testing framework by tearing down
 *          the internal state needed for running tests and freeing any
 *          associated memory. TestShutdown() should be called after any
 *          other function from this testing framework is called, and just
 *          before any optional library shutdown functions, such as H5close().
 *
 * \see TestInit()
 *
 */
H5TEST_DLL herr_t TestShutdown(void);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Prints out test program usage help text
 *
 * \param[in]  stream Pointer to output stream to direct output to
 *
 * \return void
 *
 * \details TestUsage() prints out the test program's usage help text to
 *          the given output stream specified in \p stream. This includes the
 *          general list of command-line arguments accepted by the test
 *          program, additional test program-specific usage help text printed
 *          out by the optional callback specified in TestInit() and a list
 *          of all the tests and their descriptions, as added by AddTest().
 *          \p stream may be NULL, in which case stdout is used.
 *
 *          <b>Note:</b> when a parallel test calls TestUsage(), the output,
 *          including additional output from the optional callback specified
 *          in TestInit(), is only printed from the MPI process with rank
 *          value 0. Any collective operations should currently be avoided in
 *          the optional callback if one is provided.
 *
 * \see AddTest(), TestInit()
 *
 */
H5TEST_DLL void TestUsage(FILE *stream);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Prints out miscellaneous test program information
 *
 * \param[in]  stream Pointer to output stream to direct output to
 *
 * \return void
 *
 * \details TestInfo() prints out miscellaneous information for the test
 *          program, such as the version of the HDF5 library that the program
 *          is linked against. \p stream may be NULL, in which case stdout is
 *          used.
 *
 *          <b>Note:</b> when a parallel test calls TestInfo(), the output is
 *          only printed from the MPI process with rank value 0.
 *
 */
H5TEST_DLL void TestInfo(FILE *stream);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Adds a test to the list of tests be executed
 *
 * \param[in]  TestName         The chosen name for the test to be executed
 * \param[in]  TestFunc         The function to call when executing the test
 * \param[in]  TestSetupFunc    The function to call before executing the
 *                              test
 * \param[in]  TestCleanupFunc  The function to call after executing the test
 * \param[in]  TestData         A pointer to additional data that will be
 *                              passed to the test function and its setup and
 *                              cleanup callbacks when the test runs
 * \param[in]  TestDataSize     Size of the additional test data pointed to
 *                              by \p TestData
 * \param[in]  TestDescr        A short description of the test
 *
 * \return \herr_t
 *
 * \details AddTest() adds a new test to the list of tests that will be
 *          executed when PerformTests() is called by a test program.
 *
 *          \p TestName is a short name given to a test that can be used to
 *          control how a test is executed, including skipping that test if
 *          necessary. The name specified in \p TestName must be #MAXTESTNAME
 *          bytes or less, including the NUL terminator. The name specified
 *          in \p TestName must also not be an empty string. If \p TestName
 *          begins with the character '-', the test will be set to be
 *          skipped by default.
 *
 *          \p TestFunc is a pointer to the function that will be called for
 *          the test. The function must return no value and accept a single
 *          const void * as an argument, which will point to any parameters
 *          to be passed to the test that are specified in \p TestData.
 *
 *          \p TestSetupFunc is an optional pointer to a function that will
 *          be called before the main test function is called. This allows
 *          tests to perform any pre-test setup necessary. The function must
 *          return no value and accept a single void * as an argument, which
 *          will point to any parameters to be passed to the test that are
 *          specified in \p TestData.
 *
 *          \p TestCleanupFunc is an optional pointer to a function that
 *          will be called after a test's main test function has finished
 *          executing. This allows tests to perform any post-test cleanup
 *          necessary. The function must return no value and accept a single
 *          void * as an argument, which will point to any parameters to be
 *          passed to the test that are specified in \p TestData.
 *
 *          \p TestData is an optional pointer to test parameters that will
 *          be passed to the test's main test function when executed, as well
 *          as the test's optional setup and cleanup callbacks. If given, the
 *          testing framework will make a copy of the parameters according to
 *          the size specified in \p TestDataSize. If \p TestData is not NULL,
 *          \p TestDataSize must be a positive value. Otherwise, if
 *          \p TestData is NULL, \p TestDataSize must be 0.
 *
 *          \p TestDataSize is the size of the test parameter data to be
 *          passed to the test's main function and setup and callback
 *          functions during execution. If \p TestData is not NULL,
 *          \p TestDataSize must be a positive value. Otherwise, if
 *          \p TestData is NULL, \p TestDataSize must be 0.
 *
 *          \p TestDescr is an informational description given to a test
 *          which may be printed out by the testing framework in various
 *          places. The string passed in \p TestDescr must be #MAXTESTDESC
 *          bytes or less, including the NUL terminator. The string passed
 *          in \p TestDescr may be an empty string, but it is advised that
 *          test authors give a description to a test.
 *
 * \see PerformTests()
 *
 */
H5TEST_DLL herr_t AddTest(const char *TestName, void (*TestFunc)(void *), void (*TestSetupFunc)(void *),
                          void (*TestCleanupFunc)(void *), const void *TestData, size_t TestDataSize,
                          const char *TestDescr);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Parses command-line arguments given to the test program
 *
 * \param[in]  argc Command-line argument count; received from main()
 * \param[in]  argv Command-line argument array; received from main()
 *
 * \return \herr_t
 *
 * \details TestParseCmdLine() parses the command-line arguments given to the
 *          test program. If an optional argument parsing callback was
 *          specified in the call to TestInit(), TestParseCmdLine() will
 *          defer to that function for parsing command-line arguments that
 *          it doesn't recognize. <b>Note:</b> TestParseCmdLine() requires
 *          that all standard command-line arguments must appear before any
 *          non-standard arguments that would be parsed by an optional
 *          argument parsing callback function specified in TestInit().
 *
 *          <b>Note:</b> TestParseCmdLine() should not be called until all
 *          tests have been added by AddTest() since some of the command-line
 *          arguments that are parsed involve the ability to skip certain
 *          tests.
 *
 * \see TestInit()
 *
 */
H5TEST_DLL herr_t TestParseCmdLine(int argc, char *argv[]);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Executes all tests added by AddTest() that aren't flagged to be
 *        skipped
 *
 * \return \herr_t
 *
 * \details PerformTests() runs all tests that aren't flagged to be skipped
 *          in the order added by calls to AddTest(). For each test, the
 *          test's setup callback function (if supplied) will be called
 *          first, followed by the test's primary function and then the
 *          test's cleanup callback function (if supplied). Before each test
 *          begins, a timer is enabled by a call to TestAlarmOn() to prevent
 *          the test from running longer than desired. A call to
 *          TestAlarmOff() disables this timer after each test has finished.
 *
 * \see AddTest(), TestAlarmOn()
 *
 */
H5TEST_DLL herr_t PerformTests(void);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Prints out a summary of the results of running tests
 *
 * \param[in]  stream Pointer to output stream to direct output to
 *
 * \return void
 *
 * \details TestSummary() prints out a summary of testing results, including
 *          each test's name, description and the number of errors that
 *          occurred during the test's execution. If a test was skipped, the
 *          number of errors for that test will show as "N/A". \p stream may
 *          be NULL, in which case stdout is used.
 *
 *          <b>Note:</b> when a parallel test calls TestSummary(), the output
 *          is only printed from the MPI process with rank value 0.
 *
 */
H5TEST_DLL void TestSummary(FILE *stream);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Returns the MPI rank for this process
 *
 * \return The MPI rank of this process
 *
 * \details GetTestFrameworkProcessID() returns the MPI rank for this process.
 *          Always returns rank 0 in serial HDF5.
 *
 */
H5TEST_DLL int GetTestFrameworkProcessID(void);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Returns the current test verbosity level setting
 *
 * \return The current test verbosity level setting
 *
 * \details GetTestVerbosity() returns the current setting for the level of
 *          test verbosity. These levels are as follows:
 *
 *          \snippet this test_verbo_snip
 *
 * \see SetTestVerbosity()
 *
 */
H5TEST_DLL int GetTestVerbosity(void);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Sets the current test verbosity level setting
 *
 * \return The previous test verbosity level setting
 *
 * \details SetTestVerbosity() sets a new value for the level of test
 *          verbosity and returns the previous value. These levels are as
 *          follows:
 *
 *          \snippet this test_verbo_snip
 *
 *          If \p newval is negative, the test verbosity level is set to the
 *          lowest value (VERBO_NONE). If \p newval is greater than the
 *          highest verbosity value, it is set to the highest verbosity value
 *          (VERBO_HI).
 *
 * \see GetTestVerbosity()
 *
 */
H5TEST_DLL int SetTestVerbosity(int newval);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Parses a string for a test verbosity level setting, then sets the
 *        test verbosity level to that setting
 *
 * \return \herr_t
 *
 * \details ParseTestVerbosity() parses a string for a test verbosity level
 *          setting, then sets the test verbosity level to that setting. The
 *          string may be the character 'l' (for low verbosity), 'm' (for
 *          medium verbosity), 'h' (for high verbosity) or a number between
 *          0-9, corresponding to the different predefined levels of test
 *          verbosity. If a negative number is specified, the test verbosity
 *          level is set to the default (VERBO_DEF). If a number greater
 *          than VERBO_HI is specified, the test verbosity level is set to
 *          VERBO_HI. If ParseTestVerbosity() can't parse the string, a
 *          negative value will be returned to indicate failure.
 *
 * \see GetTestVerbosity(), SetTestVerbosity()
 *
 */
H5TEST_DLL herr_t ParseTestVerbosity(char *argv);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Returns the current TestExpress setting for expedited testing
 *
 * \return The current TestExpress setting
 *
 * \details GetTestExpress() returns the current setting for the TestExpress
 *          variable which controls whether or not some testing should be
 *          expedited. The variable may be set to one of the following
 *          values:
 *
 *          0 / H5_TEST_EXPRESS_EXHAUSTIVE: Exhaustive run
 *             Tests should take as long as necessary.
 *          1 / H5_TEST_EXPRESS_FULL: Full run
 *             Default value if H5_TEST_EXPRESS_LEVEL_DEFAULT and the
 *             HDF5TestExpress environment variable are not defined.
 *             Tests should take no more than 30 minutes.
 *          2 / H5_TEST_EXPRESS_QUICK: Quick run
 *             Tests should take no more than 10 minutes.
 *          3 / H5_TEST_EXPRESS_SMOKE_TEST: Smoke test
 *             Default if the HDF5TestExpress environment variable is set to
 *             a value other than 0-3.
 *             Tests should take less than 1 minute.
 *
 *          The macro H5_TEST_EXPRESS_LEVEL_DEFAULT may be defined to one
 *          of these values at library configuration time in order to
 *          override the default value set for TestExpress. The TestExpress
 *          value may also be overridden at run time by setting the
 *          HDF5TestExpress environment variable to one of these values.
 *
 *          The limitation imposed by the TestExpress functionality applies
 *          to the total runtime of a test executable, even if it contains
 *          multiple sub-tests.
 *
 *          The standard system for test times is a Linux machine running in
 *          NFS space (to catch tests that involve a great deal of disk I/O).
 *
 * \see SetTestExpress()
 *
 */
H5TEST_DLL int GetTestExpress(void);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Sets the current TestExpress setting for expedited testing
 *
 * \return void
 *
 * \details SetTestExpress() sets a new value for the TestExpress variable
 *          which controls whether or not some testing should be expedited.
 *
 *          If \p newval is negative, the TestExpress value is set to the
 *          default value (1). If \p newval is greater than the highest
 *          TestExpress value, it is set to the highest TestExpress value
 *          (3).
 *
 * \see GetTestExpress()
 *
 */
H5TEST_DLL void SetTestExpress(int newval);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Returns the current test summary setting
 *
 * \return The current test summary setting
 *
 * \details GetTestSummary() returns whether or not a test program should
 *          call TestSummary() to print out a summary of test results after
 *          tests have run. This summary includes each test's name,
 *          description and the number of errors that occurred during the
 *          test's execution.
 *
 * \see TestSummary()
 *
 */
H5TEST_DLL bool GetTestSummary(void);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Returns the current test file cleanup status setting
 *
 * \return The current test file cleanup status setting
 *
 * \details GetTestCleanup() returns whether or not a test should clean up
 *          any temporary files it has created when it is finished running.
 *          If true is returned, the test should clean up temporary files.
 *          Otherwise, it should leave them in place.
 *
 * \see SetTestNoCleanup()
 *
 */
H5TEST_DLL bool GetTestCleanup(void);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Sets the test file cleanup status setting to "don't clean up
 *        temporary files"
 *
 * \return void
 *
 * \details SetTestNoCleanup() sets the temporary test file cleanup status
 *          to false, causing future calls to GetTestCleanup() to return
 *          false and inform tests that they should not clean up temporary
 *          test files they have created.
 *
 * \see GetTestCleanup()
 *
 */
H5TEST_DLL void SetTestNoCleanup(void);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Returns the number of errors recorded for the test program
 *
 * \return The recorded number of errors
 *
 * \details GetTestNumErrs() returns the total number of errors recorded
 *          during the execution of the test program. This number is
 *          primarily used to determine whether the test program should exit
 *          with a success or failure value.
 *
 * \see IncTestNumErrs()
 *
 */
H5TEST_DLL int GetTestNumErrs(void);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Increments the number of errors recorded for the test program
 *
 * \return void
 *
 * \details IncTestNumErrs() increments the number of errors recorded
 *          for the test program.
 *
 * \see GetTestNumErrs()
 *
 */
H5TEST_DLL void IncTestNumErrs(void);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Prints out error messages to stderr and increments the number of
 *        test program errors
 *
 * \return return value of vfprintf()
 *
 * \details TestErrPrintf() is a wrapper around vfprintf() that can be used
 *          to print out messages to stderr when a test failure occurs.
 *          TestErrPrintf() increments the number of errors recorded for the
 *          test program when called.
 *
 */
H5TEST_DLL int TestErrPrintf(const char *format, ...) H5_ATTR_FORMAT(printf, 1, 2);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Change test execution for a particular test
 *
 * \return \herr_t
 *
 * \details SetTest() is used to change how test execution occurs in relation
 *          to a particular test. \p testname is the name of the test, as
 *          specified by AddTest(), to change the behavior for. \p action
 *          should be one of the following macros:
 *
 *          SKIPTEST  - informs the testing framework to skip the test
 *                      specified by \p testname
 *          ONLYTEST  - informs the testing framework to only run the test
 *                      specified by \p testname and skip all other tests
 *          BEGINTEST - informs the testing framework to start running tests
 *                      at the test specified by \p testname and skip all
 *                      tests before it (in the order added by calls to
 *                      AddTest())
 *
 *          Other values for \p action will cause SetTest() to return
 *          a negative value for failure.
 *
 *          Multiple tests can be set to the value ONLYTEST in order to run a
 *          subset of tests. This is intended as a convenient alternative to
 *          needing to skip many other tests by setting them to the value
 *          SKIPTEST.
 *
 * \see AddTest()
 *
 */
H5TEST_DLL herr_t SetTest(const char *testname, int action);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Returns the maximum number of threads a test program is allowed to
 *        spawn in addition to the main thread
 *
 * \return The maximum number of allowed spawned threads
 *
 * \details GetTestMaxNumThreads() returns the value for the maximum number
 *          of threads a test program is allowed to spawn in addition to the
 *          main thread for the test program. This number is usually
 *          configured by a command-line argument passed to the test program
 *          and is intended for allowing tests to adjust their workload
 *          according to the resources of the testing environment.
 *
 *          The default value is -1, which means that multi-threaded tests
 *          aren't limited in the number of threads they can spawn, but
 *          should still only use a reasonable amount of threads. The value
 *          0 indicates that no additional threads should be spawned, which
 *          is primarily for testing purposes. The value returned by
 *          GetTestMaxNumThreads() is meaningless for non-multi-threaded
 *          tests.
 *
 * \see SetTestMaxNumThreads()
 *
 */
H5TEST_DLL int GetTestMaxNumThreads(void);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Sets the maximum number of threads a test program is allowed to
 *        spawn in addition to the main thread
 *
 * \return \herr_t
 *
 * \details SetTestMaxNumThreads() sets the value for the maximum number of
 *          threads a test program is allowed to spawn in addition to the
 *          main thread for the test program. This number is usually
 *          configured by a command-line argument passed to the test program
 *          and is intended for allowing tests to adjust their workload
 *          according to the resources of the testing environment.
 *
 *          If \p max_num_threads is a negative value, test programs will be
 *          allowed to spawn any number of threads, though it is advised
 *          that test programs try to limit this to a reasonable number.
 *          The value 0 indicates that no additional threads should be
 *          spawned, which is primarily for testing purposes.
 *
 * \see SetTestMaxNumThreads()
 *
 */
H5TEST_DLL herr_t SetTestMaxNumThreads(int max_num_threads);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Enables a global test timer
 *
 * \return \herr_t
 *
 * \details TestAlarmOn() enables a global test timer through use of
 *          alarm(2). This timer is intended to stop long-running or hanging
 *          tests after a configurable amount of time. The default time
 *          allowed for a test program is 1200 seconds (20 minutes). The
 *          environment variable HDF5_ALARM_SECONDS may be set to a number of
 *          seconds in order to override this value. However, a test program
 *          may still be limited by the build system used to build the
 *          library. For example, HDF5's CMake code has a default limit of
 *          1200 seconds for a test program.
 *
 *          If support for alarm(2) is not available on the system, this
 *          function has no effect.
 *
 * \see TestAlarmOff()
 *
 */
H5TEST_DLL herr_t TestAlarmOn(void);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5TEST
 *
 * \brief Disables a global test timer
 *
 * \return void
 *
 * \details TestAlarmOff() disables a global test timer as enabled by
 *          TestAlarmOn().
 *
 *          If support for alarm(2) is not available on the system, this
 *          function has no effect.
 *
 * \see TestAlarmOn()
 *
 */
H5TEST_DLL void TestAlarmOff(void);

#ifdef __cplusplus
}
#endif

#endif /* H5TESTFRAME_H */
