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
   h5cpputil.h - Header file of the utilities/misc for HDF5 C++ tests.

   EXTERNAL ROUTINES/VARIABLES:

 ***************************************************************************/

#ifndef H5cpputil_H
#define H5cpputil_H

#include "h5tools.h"

using namespace H5;
using std::cerr;
using std::endl;

class InvalidActionException : public Exception {
  public:
    InvalidActionException(const H5std_string &func_name, const H5std_string &message = DEFAULT_MSG);
    InvalidActionException();
    ~InvalidActionException() override = default;
};

class TestFailedException : public Exception {
  public:
    TestFailedException(const H5std_string &func_name, const H5std_string &message = DEFAULT_MSG);
    TestFailedException();
    ~TestFailedException() override = default;
};

/*
 * Some are duplicated from h5test.*
 */

extern size_t   n_tests_passed_g;
extern size_t   n_tests_failed_g;
extern size_t   n_tests_skipped_g;

/* Number of seconds to wait before killing a test (requires alarm(2)) */
#define H5_ALARM_SEC 1200 /* default is 20 minutes */

/*
 * Various functions for operating on the list of tests
 */
char       *h5cpp_fixname(const char *base_name, hid_t fapl, char *fullname, size_t size);
void        TestUsage(void);
void        AddTest(const char *TheName, void (*TheCall)(void), void (*Cleanup)(void),
                               const char *TheDescr);
void        TestInfo(const char *ProgName);
void        TestParseCmdLine(int argc, char *argv[]);
void        PerformTests(void);
void        TestSummary(void);
void        TestCleanup(void);
int         GetTestSummary(void);
int         GetTestCleanup(void);
int         SetTestNoCleanup(void);
int         GetTestExpress(void);
int         SetTestExpress(int newval);
int         GetTestNumErrs(void);
void        IncTestNumErrs(void);
void        TestAlarmOn(void);
void        TestAlarmOff(void);

/* Checks the HDF5_DRIVER and HDF5_TEST_DRIVER environment variables */
const char    *h5cpp_get_test_driver_name(void);

/* Return srcdir path */
const char *h5cpp_get_srcdir(void);

/* Append the test file name to the srcdir path and return the whole string */
const char *h5cpp_get_srcdir_filename(const char *filename);

/*
 * If a test passes, fails, or is skipped then the PASSED(), H5_FAILED(), or
 * SKIPPED() macro should be called.  After H5_FAILED() or SKIPPED() the caller
 * should print additional information to stdout indented by at least four
 * spaces.  If the h5_errors() is used for automatic error handling then
 * the H5_FAILED() macro is invoked automatically when an API function fails.
 */
#define PASSED()                                                                                             \
    do {                                                                                                     \
        puts(" PASSED");                                                                                     \
        fflush(stdout);                                                                                      \
        n_tests_passed_g++;                                                                                  \
    } while (0)
#define H5_FAILED()                                                                                          \
    do {                                                                                                     \
        puts("*FAILED*");                                                                                    \
        fflush(stdout);                                                                                      \
        n_tests_failed_g++;                                                                                  \
    } while (0)
#define SKIPPED()                                                                                            \
    do {                                                                                                     \
        puts(" -SKIP-");                                                                                     \
        fflush(stdout);                                                                                      \
        n_tests_skipped_g++;                                                                                 \
    } while (0)
#define SUBTEST(TEST)                                                                                        \
    do {                                                                                                     \
        printf("   Subtest: %-52s", TEST);                                                                   \
        fflush(stdout);                                                                                      \
    } while (0)

/*
 * Overloaded/Template functions to verify values and display proper info
 */

int  check_values(hsize_t i, hsize_t j, int apoint, int acheck);
void check_values(const char *value, const char *msg, int line, const char *file_name);
void display_error(const char *msg, const char *where, int line, const char *file_name);
int  test_report(int, const H5std_string &);
void issue_fail_msg(const char *where, int line, const char *file_name, const char *message = "");
void issue_fail_msg(const char *where, int line, const char *file_name, const char *func_name,
                    const char *message);

// MESSAGE from h5test.*
void MESSAGE(int num);
void MESSAGE(const char *msg);
void MESSAGE(const char *msg, const char *desc, const char *name);

// Verifies value against expected
void verify_val(const char *x, const char *value, const char *where, int line, const char *file_name, const char *var);
void verify_val(const H5std_string& x, const H5std_string& value, const char *where, int line, const char *file_name, const char *var);

template <class Type1, class Type2>
void
verify_val(Type1 x, Type2 value, const char *where, int line, const char *file_name, const char *var)
{
    if (x != value) {
        cerr << endl;
        cerr << "*** UNEXPECTED VALUE at line " << line << " from" << file_name << "::" << where
             << "(): " << var << " should be <" << value << ">, but is <" << x << ">" << endl;
        IncTestNumErrs();
        throw TestFailedException(where, "");
    }
}

template <class Type1, class Type2>
void
verify_val(Type1 x, Type2 value, float epsilon, const char *msg, int line, const char *file_name)
{
    if (x == value) {
        cerr << endl;
        cerr << "*** UNEXPECTED FLOAT VALUE: " << file_name << ":line " << line << ": " << msg
             << " different: " << x << ", should be " << value << " (epsilon=" << epsilon << ")" << endl;
        IncTestNumErrs();
        throw TestFailedException(file_name, msg);
    }
}

template <class Type1, class Type2>
void
verify_val_noteq(Type1 x, Type2 value, const char *where, int line, const char *file_name, const char *var)
{
    if (x == value) {
        cerr << endl;
        cerr << "*** UNEXPECTED VALUE at line " << line << " from " << file_name << "::" << where
             << "(): " << var << " should not be " << value << endl;
        IncTestNumErrs();
        throw TestFailedException(where, "");
    }
}

// It was already determined that the values differ (by fabs), display the message
template <class Type1, class Type2>
void
display_difference(Type1 x, Type2 value, const char *where, int line, const char *file_name, const char *var)
{
    cerr << endl;
    cerr << "*** UNEXPECTED VALUE at line " << line << " from" << file_name << "::" << where << "(): " << var
         << " should be " << value << ", but is " << x << endl;
    IncTestNumErrs();
    throw TestFailedException(where, "");
}

// Checks for failure
template <class Type1, class Type2>
void
CHECK(Type1 x, Type2 value, const char *msg, int line, const char *file_name)
{
    if (x == value) {
        cerr << endl;
        cerr << "*** Function " << msg << " FAILED at line " << line << endl;
        IncTestNumErrs();
        throw TestFailedException(file_name, msg);
    }
}

/* Prototypes for the test routines */
#ifdef __cplusplus
extern "C" {
#endif
void test_array();
void test_attr();
void test_compound();
void test_dsproplist();
void test_file();
void test_filters();
void test_links();
void test_h5s();
void test_iterate();
void test_object();
void test_reference();
void test_types();
void test_vlstrings();
void test_dset();

/* Prototypes for the cleanup routines */
void cleanup_array();
void cleanup_attr();
void cleanup_compound();
void cleanup_dsproplist();
void cleanup_dsets();
void cleanup_file();
void cleanup_filters();
void cleanup_h5s();
void cleanup_iterate();
void cleanup_links();
void cleanup_object();
void cleanup_reference();
void cleanup_types();
void cleanup_vlstrings();

#ifdef __cplusplus
}
#endif

/* not yet
void cleanup_select(void);
void cleanup_time(void);
void cleanup_vltypes(void);
void cleanup_array(void);
void cleanup_genprop(void);
void cleanup_misc(void);
*/

#endif
