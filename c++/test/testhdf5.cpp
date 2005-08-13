/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  * Copyright by the Board of Trustees of the University of Illinois.         *
  * All rights reserved.                                                      *
  *                                                                           *
  * This file is part of HDF5.  The full HDF5 copyright notice, including     *
  * terms governing use, modification, and redistribution, is contained in    *
  * the files COPYING and Copyright.html.  COPYING can be found at the root   *
  * of the source code distribution tree; Copyright.html can be found at the  *
  * root level of an installed copy of the electronic HDF5 document set and   *
  * is linked from the top-level documents page.  It can also be found at     *
  * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
  * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
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
   Each test module should include testhdf5.h and define a unique set of
   names for test files they create.

   EXTERNAL ROUTINES/VARIABLES:
	TestInit(...) -- Initialize testing framework
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

// Use C version of the header file testhdf5.h instead of re-coding it
#include "testhdf5.h"

#include "H5Cpp.h"

#ifndef H5_NO_NAMESPACE
using namespace H5;
#endif  /* !H5_NO_NAMESPACE */

int
main(int argc, char *argv[])
{
    /* Initialize testing framework */
    TestInit(argv[0], NULL, NULL);

    // testing file creation and opening in tfile.cpp
    AddTest("file", test_file, cleanup_file, "File I/O Operations", NULL);
    // testing dataspace functionalities in th5s.cpp
    AddTest("h5s",  test_h5s,  cleanup_h5s,  "Dataspaces", NULL);
    // testing attribute functionalities in tattr.cpp
    AddTest("attr", test_attr, cleanup_attr,  "Attributes", NULL);
/* Comment out tests that are not done yet. - BMR, Feb 2001
    AddTest("select", test_select, cleanup_select,  "Selections", NULL);
    AddTest("time", test_time, cleanup_time,  "Time Datatypes", NULL);
    AddTest("reference", test_reference, cleanup_reference,  "References", NULL);
    AddTest("vltypes", test_vltypes, cleanup_vltypes,  "Variable-Length Datatypes", NULL);
    AddTest("vlstrings", test_vlstrings, cleanup_vlstrings,  "Variable-Length Strings", NULL);
    AddTest("iterate", test_iterate, cleanup_iterate,  "Group & Attribute Iteration", NULL);
    AddTest("array", test_array, cleanup_array,  "Array Datatypes", NULL);
    AddTest("genprop", test_genprop, cleanup_genprop,  "Generic Properties", NULL);
Comment out tests that are not done yet */

    /* Display testing information */
    TestInfo(argv[0]);

    /* Parse command line arguments */
    TestParseCmdLine(argc,argv);

    /* Perform requested testing */
    PerformTests();

    /* Display test summary, if requested */
    if (GetTestSummary())
        TestSummary();

    /* Clean up test files, if allowed */
    if (GetTestCleanup() && !getenv("HDF5_NOCLEANUP"))
        TestCleanup();

    return (GetTestNumErrs());
}
