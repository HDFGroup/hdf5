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

/*
   FILE
   testhdf5.c - HDF5 testing framework main file.

   REMARKS
   General test wrapper for HDF5 base library test programs

   DESIGN
   Each test function should be implemented as function having no
   parameters and returning void (i.e. no return value).  They should be put
   into the list of AddTest() calls in main() below.  Functions which depend
   on other functionality should be placed below the AddTest() call for the
   base functionality testing.
   Each test module should include testhdf5.h and define a unique set of
   names for test files they create.

   BUGS/LIMITATIONS

   EXPORTED ROUTINES/VARIABLES:
   Two variables are exported: num_errs, and Verbosity.

 */

/* ANY new test needs to have a prototype in testhdf5.h */
#include "testhdf5.h"

int 
main(int argc, char *argv[])
{
    int                     Summary = 0;
    int                     CleanUp = 1;

    /* Initialize testing framework */
    TestInit();

    /* Tests are generally arranged from least to most complexity... */
    AddTest("configure", test_configure, cleanup_configure, "Configure definitions");
    AddTest("metadata", test_metadata, cleanup_metadata, "Encode/decode metadata code");
    AddTest("tbbt", test_tbbt, NULL,  "Threaded, Balanced, Binary Trees");
    AddTest("tst", test_tst, NULL,  "Ternary Search Trees");
    AddTest("heap", test_heap, NULL,  "Memory Heaps");
    AddTest("refstr", test_refstr, NULL,  "Reference Counted Strings");
    AddTest("file", test_file, cleanup_file, "Low-Level File I/O");
    AddTest("h5s",  test_h5s,  cleanup_h5s,  "Dataspaces");
    AddTest("attr", test_attr, cleanup_attr,  "Attributes");
    AddTest("select", test_select, cleanup_select,  "Selections");
    AddTest("time", test_time, cleanup_time,  "Time Datatypes");
    AddTest("reference", test_reference, cleanup_reference,  "References");
    AddTest("vltypes", test_vltypes, cleanup_vltypes,  "Variable-Length Datatypes");
    AddTest("vlstrings", test_vlstrings, cleanup_vlstrings,  "Variable-Length Strings");
    AddTest("iterate", test_iterate, cleanup_iterate,  "Group & Attribute Iteration");
    AddTest("array", test_array, cleanup_array,  "Array Datatypes");
    AddTest("genprop", test_genprop, cleanup_genprop,  "Generic Properties");
    AddTest("misc", test_misc, cleanup_misc,  "Miscellaneous");

    /* Display testing information */
    TestInfo(argv[0]);

    /* Parse command line arguments */
    TestParseCmdLine(argc,argv,&Summary,&CleanUp);

    /* Perform requested testing */
    PerformTests();

    /* Display test summary, if requested */
    if (Summary)
        TestSummary();

    /* Clean up test files, if allowed */
    if (CleanUp && !getenv("HDF5_NOCLEANUP"))
        TestCleanup();

    return (GetTestNumErrs());
}                               /* end main() */
