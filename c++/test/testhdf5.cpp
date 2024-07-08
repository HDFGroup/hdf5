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
   testhdf5.cpp - HDF5 testing framework main file.

   REMARKS
   General test wrapper for HDF5 C++ library test programs

   DESIGN
   Each test function should be implemented as function having no
   parameters and returning void (i.e. no return value).  They should be put
   into the list of AddTest() calls in main() below.  Functions which depend
   on other functionality should be placed below the AddTest() call for the
   base functionality testing.

   EXTERNAL ROUTINES/VARIABLES:
        TestInfo(...) -- Print test info
        AddTest(...)  -- Setup a test function and add it to the list of tests
        TestParseCmdLine(...) -- Parse command line arguments
        PerformTests() -- Perform requested testing
        GetTestSummary() -- Retrieve Summary request value
        TestSummary() -- Display test summary
        GetTestCleanup() -- Retrieve Cleanup request value
        TestCleanup() -- Clean up files from testing
        GetTestNumErrs() -- Retrieve the number of testing errors

 ***************************************************************************/
#include <iostream>
using std::cerr;
using std::endl;

#include <string>
#include "H5Cpp.h" // C++ API header file
using namespace H5;

#include "h5cpputil.h" // C++ utilility header file
static const char *TestProgName                     = NULL;

int
main(int argc, char *argv[])
{
    try {
    /*
     * Record the program name and private routines if provided.
     */
    TestProgName = argv[0];
 

        // testing file creation and opening in tfile.cpp
        AddTest("tfile", test_file, cleanup_file, "File I/O Operations");
        // testing dataset functionalities in dset.cpp
        AddTest("dsets", test_dset, cleanup_dsets, "Dataset I/O Operations");
        // testing dataspace functionalities in th5s.cpp
        AddTest("th5s", test_h5s, cleanup_h5s, "Dataspaces");
        // testing attribute functionalities in tattr.cpp
        AddTest("tattr", test_attr, cleanup_attr, "Attributes");
        // testing object functionalities in tobject.cpp
        AddTest("tobject", test_object, cleanup_object, "Objects");
        // testing reference functionalities in trefer.cpp
        AddTest("trefer", test_reference, cleanup_reference, "References");
        // testing variable-length strings in tvlstr.cpp
        AddTest("tvlstr", test_vlstrings, cleanup_vlstrings, "Variable-Length Strings");
        AddTest("ttypes", test_types, cleanup_types, "Generic Data Types");
        AddTest("tarray", test_array, cleanup_array, "Array Datatypes");
        AddTest("tcompound", test_compound, cleanup_compound, "Compound Data Types");
        AddTest("tdspl", test_dsproplist, cleanup_dsproplist, "Dataset Property List");
        AddTest("tfilter", test_filters, cleanup_filters, "Various Filters");
        AddTest("tlinks", test_links, cleanup_links, "Various Links");
        /* Comment out tests that are not done yet. - BMR, Feb 2001
                AddTest("select", test_select, cleanup_select,  "Selections");
                AddTest("time", test_time, cleanup_time,  "Time Datatypes");
                AddTest("vltypes", test_vltypes, cleanup_vltypes,  "Variable-Length Datatypes");
        */
        AddTest("iterate", test_iterate, cleanup_iterate, "Group & Attribute Iteration");
        /*
                AddTest("genprop", test_genprop, cleanup_genprop,  "Generic Properties");
                AddTest("id", test_ids, NULL,  "User-Created Identifiers");

        Comment out tests that are not done yet */

        /* Tentative - BMR 2007/1/12
                AddTest("enum", test_enum, cleanup_enum,  "Enum Data Types");
        */
    }
    catch (Exception &E) {
        issue_fail_msg("Tests failed", __LINE__, __FILE__, E.getCDetailMsg());
    }

    /* Display testing information */
    TestInfo(argv[0]);

    /* Parse command line arguments */
    TestParseCmdLine(argc, argv);

    // Turn off the auto-printing when failure occurs so that we can
    // handle the errors appropriately since sometime failures are caused
    // deliberately and expected, unless it is specifically requested.
    if (enable_error_stack == 0)
        Exception::dontPrint();

    /* Perform requested testing */
    PerformTests();

    /* Display test summary, if requested */
    if (GetTestSummary())
        TestSummary();

    /* Clean up test files, if allowed */
    if (GetTestCleanup() && !getenv(HDF5_NOCLEANUP))
        TestCleanup();

    return (GetTestNumErrs());
}
