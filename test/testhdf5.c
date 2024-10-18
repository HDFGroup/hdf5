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
   FILE
   testhdf5.c - HDF5 testing framework main file.

   REMARKS
   General test wrapper for HDF5 base library test programs

   DESIGN
   Each test function should be implemented as function having a single
   const void * parameter and returning void (i.e. no return value).  They
   should be put into the list of AddTest() calls in main() below.  Functions
   which depend on other functionality should be placed below the AddTest()
   call for the base functionality testing.
   Each test module should include testhdf5.h and define a unique set of
   names for test files they create.

   BUGS/LIMITATIONS


 */

/* ANY new test needs to have a prototype in testhdf5.h */
#include "testhdf5.h"

int
main(int argc, char *argv[])
{
    hid_t fapl_id = H5I_INVALID_HID;

    fapl_id = H5Pcreate(H5P_FILE_ACCESS);
    CHECK(fapl_id, H5I_INVALID_HID, "H5Pcreate");

    CHECK(H5Pget_vol_cap_flags(fapl_id, &vol_cap_flags_g), FAIL, "H5Pget_vol_cap_flags");

    H5Pclose(fapl_id);

    /* Initialize testing framework */
    if (TestInit(argv[0], NULL, NULL, NULL, NULL, 0) < 0) {
        fprintf(stderr, "couldn't initialize testing framework\n");
        exit(EXIT_FAILURE);
    }

    /* Tests are generally arranged from least to most complexity... */
    AddTest("config", test_configure, NULL, cleanup_configure, NULL, 0, "Configure definitions");
    AddTest("h5system", test_h5_system, NULL, cleanup_h5_system, NULL, 0, "H5system routines");
    AddTest("metadata", test_metadata, NULL, cleanup_metadata, NULL, 0, "Encoding/decoding metadata");
    AddTest("checksum", test_checksum, NULL, cleanup_checksum, NULL, 0, "Checksum algorithm");
    AddTest("skiplist", test_skiplist, NULL, NULL, NULL, 0, "Skip Lists");
    AddTest("refstr", test_refstr, NULL, NULL, NULL, 0, "Reference Counted Strings");
    AddTest("file", test_file, NULL, cleanup_file, NULL, 0, "Low-Level File I/O");
    AddTest("objects", test_h5o, NULL, cleanup_h5o, NULL, 0, "Generic Object Functions");
    AddTest("h5s", test_h5s, NULL, cleanup_h5s, NULL, 0, "Dataspaces");
    AddTest("coords", test_coords, NULL, cleanup_coords, NULL, 0, "Dataspace coordinates");
    AddTest("sohm", test_sohm, NULL, cleanup_sohm, NULL, 0, "Shared Object Header Messages");
    AddTest("attr", test_attr, NULL, cleanup_attr, NULL, 0, "Attributes");
    AddTest("select", test_select, NULL, cleanup_select, NULL, 0, "Selections");
    AddTest("time", test_time, NULL, cleanup_time, NULL, 0, "Time Datatypes");
    AddTest("ref_deprec", test_reference_deprec, NULL, cleanup_reference_deprec, NULL, 0,
            "Deprecated References");
    AddTest("ref", test_reference, NULL, cleanup_reference, NULL, 0, "References");
    AddTest("vltypes", test_vltypes, NULL, cleanup_vltypes, NULL, 0, "Variable-Length Datatypes");
    AddTest("vlstrings", test_vlstrings, NULL, cleanup_vlstrings, NULL, 0, "Variable-Length Strings");
    AddTest("iterate", test_iterate, NULL, cleanup_iterate, NULL, 0, "Group & Attribute Iteration");
    AddTest("array", test_array, NULL, cleanup_array, NULL, 0, "Array Datatypes");
    AddTest("genprop", test_genprop, NULL, cleanup_genprop, NULL, 0, "Generic Properties");
    AddTest("unicode", test_unicode, NULL, cleanup_unicode, NULL, 0, "UTF-8 Encoding");
    AddTest("id", test_ids, NULL, NULL, NULL, 0, "User-Created Identifiers");
    AddTest("misc", test_misc, NULL, cleanup_misc, NULL, 0, "Miscellaneous");

    /* Display testing information */
    TestInfo(stdout);

    /* Parse command line arguments */
    if (TestParseCmdLine(argc, argv) < 0) {
        fprintf(stderr, "couldn't parse command-line arguments\n");
        TestShutdown();
        exit(EXIT_FAILURE);
    }

    /* Perform requested testing */
    if (PerformTests() < 0) {
        fprintf(stderr, "couldn't run tests\n");
        TestShutdown();
        exit(EXIT_FAILURE);
    }

    /* Display test summary, if requested */
    if (GetTestSummary())
        TestSummary(stdout);

    /* Release test infrastructure */
    if (TestShutdown() < 0) {
        fprintf(stderr, "couldn't shut down testing framework\n");
        exit(EXIT_FAILURE);
    }

    /* Exit failure if errors encountered; else exit success. */
    /* No need to print anything since PerformTests() already does. */
    if (GetTestNumErrs() > 0)
        exit(EXIT_FAILURE);
    else
        exit(EXIT_SUCCESS);
} /* end main() */
