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

/*****************************************************************************
   FILE
   h5cpputil.h - Header file of the utilities/misc for HDF5 C++ tests.

   EXTERNAL ROUTINES/VARIABLES:

 ***************************************************************************/

#ifndef H5cpputil_H
#define H5cpputil_H

#include "h5test.h"
#include "testframe.h"

using namespace H5;
using std::cerr;
using std::endl;

#define SUBTEST(TEST)                                                                                        \
    do {                                                                                                     \
        printf("   Subtest: %-52s", TEST);                                                                   \
        fflush(stdout);                                                                                      \
    } while (0)

int  check_values(hsize_t i, hsize_t j, int apoint, int acheck);
void check_values(const char *value, const char *msg, int line, const char *file_name);
int  test_report(int, const H5std_string &);
void issue_fail_msg(const char *where, int line, const char *file_name, const char *message = "");
void issue_fail_msg(const char *where, int line, const char *file_name, const char *func_name,
                    const char *message);

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

// Overloaded/Template functions to verify values and display proper info

// Verifies
void verify_val(const char *x, const char *value, const char *where, int line, const char *file_name);

template <class Type1, class Type2>
void
verify_val(Type1 x, Type2 value, const char *where, int line, const char *file_name)
{
    if (GetTestVerbosity() >= VERBO_HI) {
        cerr << endl;
        cerr << "   Call to routine: " << where << " at line " << line << " in " << file_name << " had value "
             << x << endl;
    }
    if (x != value) {
        cerr << endl;
        cerr << "*** UNEXPECTED VALUE from " << where << " should be " << value << ", but is " << x
             << " at line " << line << " in " << file_name << endl;
        IncTestNumErrs();
        throw TestFailedException(where, "");
    }
}

template <class Type1, class Type2>
void
verify_val(Type1 x, Type2 value, const char *msg, const char *file_name, int line)
{
    if (x != value) {
        cerr << endl;
        cerr << "*** UNEXPECTED VALUE: " << file_name << ":line " << line << ": " << msg
             << " different: " << x << ", should be " << value << endl;
        IncTestNumErrs();
        throw TestFailedException(file_name, msg);
    }
}

template <class Type1, class Type2>
void
verify_val_noteq(Type1 x, Type2 value, const char *where, int line, const char *file_name)
{
    if (GetTestVerbosity() >= VERBO_HI) {
        cerr << endl;
        cerr << "   Call to routine: " << where << " at line " << line << " in " << file_name << " had value "
             << x << endl;
    }
    if (x == value) {
        cerr << endl;
        cerr << "*** UNEXPECTED VALUE from " << where << " should not be " << value << " at line " << line
             << " in " << file_name << endl;
        IncTestNumErrs();
        throw TestFailedException(where, "");
    }
}

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

/* Prototypes for the test routines */
#ifdef __cplusplus
extern "C" {
#endif
void test_array(void *params);
void test_attr(void *params);
void test_compound(void *params);
void test_dsproplist(void *params);
void test_file(void *params);
void test_filters(void *params);
void test_links(void *params);
void test_h5s(void *params);
void test_iterate(void *params);
void test_object(void *params);
void test_reference(void *params);
void test_types(void *params);
void test_vlstrings(void *params);
void test_dset(void *params);

/* Prototypes for the cleanup routines */
void cleanup_array(void *params);
void cleanup_attr(void *params);
void cleanup_compound(void *params);
void cleanup_dsproplist(void *params);
void cleanup_dsets(void *params);
void cleanup_file(void *params);
void cleanup_filters(void *params);
void cleanup_h5s(void *params);
void cleanup_iterate(void *params);
void cleanup_links(void *params);
void cleanup_object(void *params);
void cleanup_reference(void *params);
void cleanup_types(void *params);
void cleanup_vlstrings(void *params);

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
