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
        TestInit(...) -- Initialize testing framework
        TestInfo(...) -- Print test info
        AddTest(...)  -- Setup a test function and add it to the list of tests
        TestParseCmdLine(...) -- Parse command line arguments
        PerformTests() -- Perform requested testing
        GetTestSummary() -- Retrieve Summary request value
        TestSummary() -- Display test summary
        GetTestNumErrs() -- Retrieve the number of testing errors

 ***************************************************************************/
#include <iostream>
using std::cerr;
using std::endl;

#include <string>
#include "H5Cpp.h" // C++ API header file
using namespace H5;

#include "h5test.h"
#include "h5cpputil.h" // C++ utilility header file

int
main(int argc, char *argv[])
{
    try {
        // Turn of the auto-printing when failure occurs so that we can
        // handle the errors appropriately since sometime failures are
        // caused deliberately and expected.
        Exception::dontPrint();
        /* Initialize testing framework */
        TestInit(argv[0], NULL, NULL, NULL, NULL, 0);

        // testing file creation and opening in tfile.cpp
        AddTest("tfile", test_file, NULL, cleanup_file, NULL, 0, "File I/O Operations");
        // testing dataset functionalities in dset.cpp
        AddTest("dsets", test_dset, NULL, cleanup_dsets, NULL, 0, "Dataset I/O Operations");
        // testing dataspace functionalities in th5s.cpp
        AddTest("th5s", test_h5s, NULL, cleanup_h5s, NULL, 0, "Dataspaces");
        // testing attribute functionalities in tattr.cpp
        AddTest("tattr", test_attr, NULL, cleanup_attr, NULL, 0, "Attributes");
        // testing object functionalities in tobject.cpp
        AddTest("tobject", test_object, NULL, cleanup_object, NULL, 0, "Objects");
        // testing reference functionalities in trefer.cpp
        AddTest("trefer", test_reference, NULL, cleanup_reference, NULL, 0, "References");
        // testing variable-length strings in tvlstr.cpp
        AddTest("tvlstr", test_vlstrings, NULL, cleanup_vlstrings, NULL, 0, "Variable-Length Strings");
        AddTest("ttypes", test_types, NULL, cleanup_types, NULL, 0, "Generic Data Types");
        AddTest("tarray", test_array, NULL, cleanup_array, NULL, 0, "Array Datatypes");
        AddTest("tcompound", test_compound, NULL, cleanup_compound, NULL, 0, "Compound Data Types");
        AddTest("tdspl", test_dsproplist, NULL, cleanup_dsproplist, NULL, 0, "Dataset Property List");
        AddTest("tfilter", test_filters, NULL, cleanup_filters, NULL, 0, "Various Filters");
        AddTest("tlinks", test_links, NULL, cleanup_links, NULL, 0, "Various Links");
        /* Comment out tests that are not done yet. - BMR, Feb 2001
                AddTest("select", test_select, NULL, cleanup_select, NULL, 0, "Selections");
                AddTest("time", test_time, NULL, cleanup_time, NULL, 0, "Time Datatypes");
                AddTest("vltypes", test_vltypes, NULL, cleanup_vltypes, NULL, 0, "Variable-Length Datatypes");
        */
        AddTest("iterate", test_iterate, NULL, cleanup_iterate, NULL, 0, "Group & Attribute Iteration");
        /*
                AddTest("genprop", test_genprop, NULL, cleanup_genprop, NULL, 0, "Generic Properties");
                AddTest("id", test_ids, NULL, NULL, NULL, 0, "User-Created Identifiers");

        Comment out tests that are not done yet */

        /* Tentative - BMR 2007/1/12
                AddTest("enum", test_enum, NULL, cleanup_enum, NULL, 0, "Enum Data Types");
        */
    }
    catch (Exception &E) {
        issue_fail_msg("Tests failed", __LINE__, __FILE__, E.getCDetailMsg());
    }

    /* Display testing information */
    TestInfo(stdout);

    /* Parse command line arguments */
    TestParseCmdLine(argc, argv);

    /* Perform requested testing */
    PerformTests();

    /* Display test summary, if requested */
    if (GetTestSummary())
        TestSummary(stdout);

    /* Release test infrastructure */
    TestShutdown();

    return (GetTestNumErrs());
}
