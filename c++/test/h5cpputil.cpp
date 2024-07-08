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

/*****************************************************************************
   FILE
   util.cpp - Utility routines for HDF5 C++ tests.

   EXTERNAL ROUTINES/VARIABLES:

 ***************************************************************************/

#include <iostream>
#include <vector>
#include <string>
using namespace std;

#include "H5Cpp.h" // C++ API header file
using namespace H5;

#include "h5cpputil.h" // C++ utilility header file
#include "H5srcdir_str.h"
static char srcdir_path[1024];
static char srcdir_testpath[1024];

bool   subst_for_superblock = false;
size_t n_tests_passed_g     = 0;
size_t n_tests_failed_g     = 0;
size_t n_tests_skipped_g    = 0;

/*
 * Definitions for the testing structure.
 */
#define MAXTESTNAME 16
#define MAXTESTDESC 64

typedef struct TestStruct {
    int  NumErrors;
    char Description[MAXTESTDESC];
    int  SkipFlag;
    char Name[MAXTESTNAME];
    void (*Call)(void);
    void (*Cleanup)(void);
} TestStruct;

/*
 * Variables used by testing framework.
 */
static int         num_errs     = 0; /* Total number of errors during testing */
static int         Summary      = 0; /* Show test summary. Default is no. */
static int         CleanUp      = 1; /* Do cleanup or not. Default is yes. */
static const char *TestProgName = NULL;
vector<TestStruct> TestList; /* Array of tests */

/*-------------------------------------------------------------------------
 * Function:    MESSAGE
 *
 * Purpose      Prints out the number of errors for the tests indicated
 *              by 'testname,' if there were any failures occurred.  If
 *              no failure, MESSAGE prints out the tests passed message.
 *
 * Return       if any failure has occurred: 1
 *
 *              if no failure occurs: 0
 *-------------------------------------------------------------------------
 */
void
MESSAGE(int nerrors)
{
    cerr << "There were " << nerrors << " errors detected." << endl;
    cerr << endl;
}
void
MESSAGE(const char *msg)
{
    cerr << msg << endl;
}
void
MESSAGE(const char *msg, const char *desc, const char *name)
{
    cerr << msg << " -- " << desc << "(" << name << ")" << endl;
}

/*-------------------------------------------------------------------------
 * Function:    test_report
 *
 * Purpose      Prints out the number of errors for the tests indicated
 *              by 'testname,' if there were any failures occurred.  If
 *              no failure, test_report prints out the tests passed message.
 *
 * Return       if any failure has occurred: 1
 *
 *              if no failure occurs: 0
 *-------------------------------------------------------------------------
 */
int
test_report(int nerrors, const H5std_string &testname)
{
    if (nerrors) {
        nerrors = MAX(1, nerrors);
        if (1 == nerrors)
            cerr << "***** " << nerrors << testname << " TEST FAILED! *****" << endl;
        else
            cerr << "***** " << nerrors << testname << " TESTS FAILED! *****" << endl;
        return 1;
    }
    else {
        cerr << "All" << testname << " tests passed." << endl;
        return 0;
    }
}

/*-------------------------------------------------------------------------
 * Function:    issue_fail_msg
 *
 * Purpose      Displays that a function has failed with its location.
 *
 * Return       None
 *-------------------------------------------------------------------------
 */
void
issue_fail_msg(const char *where, int line, const char *file_name, const char *message)
{
    cerr << endl;
    cerr << ">>> FAILED in " << where << " at line " << line << " in " << file_name << " - " << message
         << endl
         << endl;
}

/*-------------------------------------------------------------------------
 * Function:    issue_fail_msg
 *
 * Purpose      Displays that a function has failed with its location, including
 *              the function name.
 *
 * Return       None
 *-------------------------------------------------------------------------
 */
void
issue_fail_msg(const char *where, int line, const char *file_name, const char *func_name, const char *message)
{
    cerr << endl;
    cerr << ">>> FAILED in " << where << ": " << func_name << endl
         << "    at line " << line << " in " << file_name << endl
         << "    C library detail: " << message << endl
         << endl;
}

/*-------------------------------------------------------------------------
 * Function:    check_values
 *
 * Purpose      Checks a read value against the written value.  If they are
 *              different, the function will print out a message and the
 *              different values.  This function is made to reuse the code
 *              segment that is used in various places throughout
 *              the test code.  Where the C version of this code segment
 *              "goto error," this function will return -1.
 *
 * Return       Success: 0
 *
 *              Failure: -1
 *-------------------------------------------------------------------------
 */
int
check_values(hsize_t i, hsize_t j, int apoint, int acheck)
{
    if (apoint != acheck) {
        cerr << "    Read different values than written.\n" << endl;
        cerr << "    At index " << static_cast<unsigned long>(i) << "," << static_cast<unsigned long>(j)
             << endl;
        return -1;
    }
    return 0;
} // check_values

/*-------------------------------------------------------------------------
 * Function:    check_values
 *
 * Purpose      Checks a char string pointer for NULL.  If it is NULL,
 *              the function will print out a message
 *
 * Return       Success: 0
 *
 *              Failure: -1
 *-------------------------------------------------------------------------
 */
void
check_values(const char *value, const char *msg, int line, const char *file_name)
{
    if (value == NULL) {
        cerr << endl;
        cerr << "*** ERROR: " << msg << ", at line " << line << endl;
        IncTestNumErrs();
        throw TestFailedException(file_name, msg);
    }
}

/*-------------------------------------------------------------------------
 * Function:    display_error
 *
 * Purpose      Displays a error's message and its location.
 *
 * Return       None
 *-------------------------------------------------------------------------
 */
void
display_error(const char *msg, const char *where, int line, const char *file_name)
{
    cerr << endl;
    cerr << "*** ERROR at line " << line << " in " << file_name << "::" << where << "(): " << msg << endl;
    IncTestNumErrs();
    throw TestFailedException(where, "");
}

/*-------------------------------------------------------------------------
 * Function:    verify_val (const char*, const char*,...)
 *
 * Purpose      Compares two character strings.  If they are
 *              different, the function will print out a message and the
 *              different values.
 *
 * Return       None
 *-------------------------------------------------------------------------
 */
void
verify_val(const char *x, const char *value, const char *where, int line, const char *file_name,
           const char *var)
{
    /* cerr << endl << "this verify_val const char *x and const char *value" << endl;
     */
    if (strcmp(x, value) != 0) {
        cerr << endl;
        cerr << "*** UNEXPECTED VALUE at line " << line << " from " << file_name << "::" << where
             << "(): " << var << " should be " << value << endl;
        // IncTestNumErrs();
        throw TestFailedException(where, "");
    }
}

/*-------------------------------------------------------------------------
 * Function:    verify_val (const string&, const string&,...)
 *
 * Purpose      Compares two strings.  If they are different, the
 *              function will print out a message and the different values.
 *
 * Return       None
 *-------------------------------------------------------------------------
 */
void
verify_val(const H5std_string &x, const H5std_string &value, const char *where, int line,
           const char *file_name, const char *var)
{
    /* cerr << endl << "this verify_val const H5std_string& x and const H5std_string& value" << endl;
     */
    verify_val(x.c_str(), value.c_str(), where, line, file_name, var);
}

//--------------------------------------------------------------------------
// Function:    InvalidActionException default constructor
//--------------------------------------------------------------------------
InvalidActionException::InvalidActionException() : Exception()
{
}

//--------------------------------------------------------------------------
// Function:    InvalidActionException overloaded constructor
//
// Purpose      Creates an InvalidActionException with the name of the function,
//              which the failure should have occurred but didn't, and a
//              message explaining why it should fail.
// Parameters
//              func    - IN: Name of the function where failure should occur
//              message - IN: Message
//--------------------------------------------------------------------------
InvalidActionException::InvalidActionException(const H5std_string &func, const H5std_string &message)
    : Exception(func, message)
{
}

//--------------------------------------------------------------------------
// Function:    TestFailedException default constructor
//--------------------------------------------------------------------------
TestFailedException::TestFailedException() : Exception()
{
}

//--------------------------------------------------------------------------
// Function:    TestFailedException overloaded constructor
//
// Purpose      Creates an TestFailedException with the name of the function,
//              which the failure should have occurred but didn't, and a
//              message explaining why it should fail.
// Parameters
//              func    - IN: Name of the function where failure should occur
//              message - IN: Message
//--------------------------------------------------------------------------
TestFailedException::TestFailedException(const H5std_string &func, const H5std_string &message)
    : Exception(func, message)
{
}

/*
 * Setup a test function and add it to the list of tests.
 *      It must have no parameters and returns void.
 * TheName--short test name.
 *    If the name starts with '-', do not run it by default.
 * TheCall--the test routine.
 * Cleanup--the cleanup routine for the test.
 * TheDescr--Long description of the test.
 *    Since only the pointer is copied, the contents should not change.
 * Return: Void
 *    exit EXIT_FAILURE if error is encountered.
 */
void
AddTest(const char *TheName, void (*TheCall)(void), void (*Cleanup)(void), const char *TheDescr)
{
    /* Sanity checking */
    if (strlen(TheDescr) >= MAXTESTDESC) {
        printf("Test description ('%s') too long, increase MAXTESTDESC(%d).\n", TheDescr, MAXTESTDESC);
        exit(EXIT_FAILURE);
    }
    if (strlen(TheName) >= MAXTESTNAME) {
        printf("Test name too long, increase MAXTESTNAME(%d).\n", MAXTESTNAME);
        exit(EXIT_FAILURE);
    }

    struct TestStruct aTest;

    /* Set up test function */
    strcpy(aTest.Description, TheDescr);
    if (*TheName != '-') {
        strcpy(aTest.Name, TheName);
        aTest.SkipFlag = 0;
    }
    else { /* skip test by default */
        strcpy(aTest.Name, TheName + 1);
        aTest.SkipFlag = 1;
    }
    aTest.Call      = TheCall;
    aTest.Cleanup   = Cleanup;
    aTest.NumErrors = -1;

    // Insertion of an element using push_back()
    TestList.push_back(aTest);
}

/*
 * Print test usage.
 *    First print the common test options, then the extra options if provided.
 */
void
TestUsage(void)
{
    cerr << "Usage: " << TestProgName << endl;
    cerr << "              [-s[ummary]]" << endl;
    cerr << "              [-c[leanoff]]" << endl;
    cerr << "              [-h[elp]]" << endl;
    cerr << endl << endl;
    cerr << "summary   prints a summary of test results at the end" << endl;
    cerr << "cleanoff  does not delete *.h5 files after execution of tests" << endl;
    cerr << "help      print out this information" << endl;
    cerr << endl << endl;
    cerr << "This program currently tests the following:" << endl << endl;
    cerr << "Name"
         << "    "
         << "Description" << endl;
    cerr << "----"
         << "    "
         << "-----------" << endl;
    for (auto &aTest : TestList)
        cerr << aTest.Name << " -- " << aTest.Description << endl;
    cerr << endl;
}

/*
 * Print test info.
 */
void
TestInfo(const char *ProgName)
{
    unsigned major, minor, release;

    H5get_libversion(&major, &minor, &release);

    cerr << "\nFor help use: " << ProgName << " -help\n" << endl;
    cerr << "Linked with hdf5 version " << major << "." << minor << " release " << release << endl;
}

/*
 * Parse command line information.
 *      argc, argv: the usual command line argument count and strings
 * The C++ test only has one argument: -h/-help
 *
 * Return: void
 *    exit EXIT_FAILURE if error is encountered.
 */
void
TestParseCmdLine(int argc, char *argv[])
{
    while ((void)argv++, --argc > 0) {
        if ((strcmp(*argv, "-help") == 0) || (strcmp(*argv, "-h") == 0)) {
            TestUsage();
            exit(EXIT_SUCCESS);
        }
        else if ((strcmp(*argv, "-summary") == 0) || (strcmp(*argv, "-s") == 0))
            Summary = 1;
        else if (strcmp(*argv, "-enable-error-stack") == 0)
            enable_error_stack = 1;
        else if ((strcmp(*argv, "-cleanoff") == 0) || (strcmp(*argv, "-c") == 0))
            SetTestNoCleanup();
        else {
            /* non-standard option.  Break out. */
            break;
        }
    }
}

/*
 * Perform Tests.
 */
void
PerformTests(void)
{
    for (auto &aTest : TestList) {
        if (aTest.SkipFlag) {
            MESSAGE("Skipping", aTest.Description, aTest.Name);
        }
        else {
            MESSAGE("Testing", aTest.Description, aTest.Name);
            MESSAGE(("===============================================\n"));
            aTest.NumErrors = num_errs;
            TestAlarmOn();
            aTest.Call();
            TestAlarmOff();
            aTest.NumErrors = num_errs - aTest.NumErrors;
            MESSAGE(("===============================================\n"));
            MESSAGE(aTest.NumErrors);
        }
    }

    MESSAGE(("\n\n"));
    if (num_errs)
        cerr << "!!! " << static_cast<int>(num_errs) << " Error(s) were detected !!!" << endl << endl;
    else
        cerr << "All tests were successful." << endl << endl;
}

/*
 * Display test summary.
 */
void
TestSummary(void)
{
    cerr << "Summary of Test Results:" << endl;
    cerr << "Name of Test     Errors Description of Test" << endl;
    cerr << "---------------- ------ --------------------------------------" << endl;

    for (auto &aTest : TestList) {
        if (aTest.NumErrors == -1)
            cerr << aTest.Name << "N/A" << aTest.Description << endl;
        else {
            cerr << aTest.Name << static_cast<int>(aTest.NumErrors) << aTest.Description << endl;
        }
    }
    cerr << endl << endl;
}

/*
 * Cleanup files from testing
 */
void
TestCleanup(void)
{
    MESSAGE(("\nCleaning Up temp files...\n\n"));

    /* call individual cleanup routines in each source module */
    for (auto &aTest : TestList)
        if (!aTest.SkipFlag && aTest.Cleanup != NULL)
            aTest.Cleanup();
}

/*
 * Retrieve Summary request value.
 *     0 means no summary, 1 means yes.
 */
H5_ATTR_PURE int
GetTestSummary(void)
{
    return (Summary);
}

/*
 * Retrieve Cleanup request value.
 *     0 means no Cleanup, 1 means yes.
 */
H5_ATTR_PURE int
GetTestCleanup(void)
{
    return (CleanUp);
}

/*
 * Set cleanup to no.
 * Return previous cleanup value.
 */
int
SetTestNoCleanup(void)
{
    int oldval;

    oldval  = CleanUp;
    CleanUp = 0;
    return (oldval);
}

/*
 * Retrieve the number of testing errors for the testing framework
 */
H5_ATTR_PURE int
GetTestNumErrs(void)
{
    return (num_errs);
}

/*
 * Increment the number of testing errors
 */
void
IncTestNumErrs(void)
{
    num_errs++;
}

char *
h5cpp_fixname(const char *base_name, hid_t fapl, char *fullname, size_t size)
{
    const char *prefix         = NULL;
    const char *driver_env_var = NULL; /* HDF5_DRIVER/HDF5_TEST_DRIVER environment variable     */
    char       *ptr, last = '\0';
    const char *suffix = ".h5";
    size_t      i, j;
    hid_t       driver     = H5I_INVALID_HID;
    bool        isppdriver = false; /* if the driver is MPI parallel */

    if (!base_name || !fullname || size < 1)
        return NULL;

    memset(fullname, 0, size);

    /* Determine if driver is set by environment variable. If it is,
     * only generate a suffix if fixing the filename for the superblock
     * file. */
    driver_env_var = h5cpp_get_test_driver_name();
    if (driver_env_var && (H5P_DEFAULT == fapl) && subst_for_superblock)
        fapl = H5P_FILE_ACCESS_DEFAULT;

    /* figure out the suffix */
    if (H5P_DEFAULT != fapl) {
        if ((driver = H5Pget_driver(fapl)) < 0)
            return NULL;

        if (suffix) {
            if (H5FD_FAMILY == driver) {
                if (subst_for_superblock)
                    suffix = "-000000.h5";
                else {
                    suffix = "-%06d.h5";
                }
            }
            else if (H5FD_MULTI == driver) {

                /* Check the HDF5_DRIVER/HDF5_TEST_DRIVER environment
                 * variable in case we are using the split driver since
                 * both of those use the multi VFD under the hood.
                 */
                if (driver_env_var && !strcmp(driver_env_var, "split")) {
                    /* split VFD */
                    if (subst_for_superblock)
                        suffix = ".h5.meta";
                    else
                        suffix = NULL;
                }
                else {
                    /* multi VFD */
                    if (subst_for_superblock)
                        suffix = "-s.h5";
                    else
                        suffix = NULL;
                }
            }
        }
    }

    /* Check HDF5_NOCLEANUP environment setting. */
    if (getenv(HDF5_NOCLEANUP))
        SetTestNoCleanup();

    /* Check what prefix to use for test files. Process HDF5_PREFIX. */
    /*
     * First use the environment variable, then try the constant
     */
    prefix = getenv("HDF5_PREFIX");

#ifdef HDF5_PREFIX
    if (!prefix)
        prefix = HDF5_PREFIX;
#endif /* HDF5_PREFIX */

    /* Prepend the prefix value to the base name */
    if (prefix && *prefix) {
        if (isppdriver) {
            if (!fullname[0]) {
                /* We didn't append the prefix yet */
                strncpy(fullname, prefix, size);
                fullname[size - 1] = '\0';
            }

            if (strlen(fullname) + strlen(base_name) + 1 < size) {
                /*
                 * Append the base_name with a slash first. Multiple
                 * slashes are handled below.
                 */
                h5_stat_t buf;

                memset(&buf, 0, sizeof(h5_stat_t));
                if (HDstat(fullname, &buf) < 0)
                    /* The directory doesn't exist just yet */
                    if (HDmkdir(fullname, static_cast<mode_t>(0755)) < 0 && errno != EEXIST)
                        /*
                         * We couldn't make the "/tmp/${USER,LOGIN}"
                         * subdirectory.  Default to PREFIX's original
                         * prefix value.
                         */
                        strcpy(fullname, prefix);

                strcat(fullname, "/");
                strcat(fullname, base_name);
            }
            else {
                /* Buffer is too small */
                return NULL;
            }
        }
        else {
            if (snprintf(fullname, size, "%s/%s", prefix, base_name) == static_cast<int>(size))
                /* Buffer is too small */
                return NULL;
        }
    }
    else if (strlen(base_name) >= size) {
        /* Buffer is too small */
        return NULL;
    }
    else {
        strcpy(fullname, base_name);
    }

    /* Append a suffix */
    if (suffix) {
        if (strlen(fullname) + strlen(suffix) >= size)
            return NULL;

        strcat(fullname, suffix);
    }

    /* Remove any double slashes in the filename */
    for (ptr = fullname, i = j = 0; ptr && i < size; i++, ptr++) {
        if (*ptr != '/' || last != '/')
            fullname[j++] = *ptr;

        last = *ptr;
    }

    return fullname;
}

/*-------------------------------------------------------------------------
 * Function:    h5cpp_get_test_driver_name
 *
 * Purpose:     Checks the HDF5_DRIVER and HDF5_TEST_DRIVER environment
 *              variables to see if a driver name has been set for testing.
 *
 * Return:      Driver name if set/NULL otherwise
 *
 *-------------------------------------------------------------------------
 */
const char *
h5cpp_get_test_driver_name(void)
{
    char *envval;

    assert(H5_DEFAULT_VFD == H5FD_SEC2);

    if ((envval = getenv(HDF5_DRIVER)))
        return envval;
    else if ((envval = getenv("HDF5_TEST_DRIVER")))
        return envval;
    else
        return H5_DEFAULT_VFD_NAME;
}

/* Enable a test timer that will kill long-running tests, the time is configurable
 * via an environment variable.
 *
 * Only useful on POSIX systems where alarm(2) is present. This does not include
 * MinGW builds, which will often incorrectly decide that alarm(2) exists.
 */
void
TestAlarmOn(void)
{
#ifdef H5_HAVE_ALARM
    char         *env_val   = getenv("HDF5_ALARM_SECONDS"); /* Alarm environment */
    unsigned long alarm_sec = H5_ALARM_SEC;                 /* Number of seconds before alarm goes off */

    /* Get the alarm value from the environment variable, if set */
    if (env_val != NULL)
        alarm_sec = static_cast<unsigned>(strtoul(env_val, (char **)NULL, 10));

    /* Set the number of seconds before alarm goes off */
    alarm(static_cast<unsigned>(alarm_sec));
#endif
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

/*-------------------------------------------------------------------------
 * Function:    h5cpp_get_srcdir
 *
 * Purpose:     Just return the srcdir path
 *
 * Return:      The string
 *
 *-------------------------------------------------------------------------
 */
const char *
h5cpp_get_srcdir(void)
{
    const char *srcdir = getenv("srcdir");

    /* Check for using the srcdir from configure time */
    if (NULL == srcdir)
        srcdir = config_srcdir;

    /* Build path to all test files */
    if ((strlen(srcdir) + 2) < sizeof(srcdir_path)) {
        snprintf(srcdir_path, sizeof(srcdir_path), "%s/", srcdir);
        return (srcdir_path);
    } /* end if */
    else
        return (NULL);
} /* end h5cpp_get_srcdir() */

/*-------------------------------------------------------------------------
 * Function:    h5cpp_get_srcdir_filename
 *
 * Purpose:     Append the test file name to the srcdir path and return the whole string
 *
 * Return:      The string or NULL (errors or not enough space)
 *
 *-------------------------------------------------------------------------
 */
const char *
h5cpp_get_srcdir_filename(const char *filename)
{
    const char *srcdir = h5cpp_get_srcdir();

    /* Check for error */
    if (NULL == srcdir)
        return NULL;

    /* Build path to test file. We're checking the length so suppress
     * the gcc format-truncation warning.
     */
    if ((strlen(srcdir) + strlen("testfiles/") + strlen(filename) + 1) < sizeof(srcdir_testpath)) {
        H5_GCC_DIAG_OFF("format-truncation")
        snprintf(srcdir_testpath, sizeof(srcdir_testpath), "%s%s", srcdir, filename);
        H5_GCC_DIAG_ON("format-truncation")
        return srcdir_testpath;
    }

    /* If not enough space, just return NULL */
    return NULL;
} /* end h5cpp_get_srcdir_filename() */
