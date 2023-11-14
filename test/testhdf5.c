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
    TestInit(argv[0], NULL, NULL);

    /* Tests are generally arranged from least to most complexity... */
    AddTest("config", test_configure, cleanup_configure, "Configure definitions", NULL);
    AddTest("h5system", test_h5_system, cleanup_h5_system, "H5system routines", NULL);
    AddTest("metadata", test_metadata, cleanup_metadata, "Encoding/decoding metadata", NULL);
    AddTest("checksum", test_checksum, cleanup_checksum, "Checksum algorithm", NULL);
    AddTest("skiplist", test_skiplist, NULL, "Skip Lists", NULL);
    AddTest("refstr", test_refstr, NULL, "Reference Counted Strings", NULL);
    AddTest("file", test_file, cleanup_file, "Low-Level File I/O", NULL);
    AddTest("objects", test_h5o, cleanup_h5o, "Generic Object Functions", NULL);
    AddTest("h5s", test_h5s, cleanup_h5s, "Dataspaces", NULL);
    AddTest("coords", test_coords, cleanup_coords, "Dataspace coordinates", NULL);
    AddTest("sohm", test_sohm, cleanup_sohm, "Shared Object Header Messages", NULL);
    AddTest("attr", test_attr, cleanup_attr, "Attributes", NULL);
    AddTest("select", test_select, cleanup_select, "Selections", NULL);
    AddTest("time", test_time, cleanup_time, "Time Datatypes", NULL);
    AddTest("ref_deprec", test_reference_deprec, cleanup_reference_deprec, "Deprecated References", NULL);
    AddTest("ref", test_reference, cleanup_reference, "References", NULL);
    AddTest("vltypes", test_vltypes, cleanup_vltypes, "Variable-Length Datatypes", NULL);
    AddTest("vlstrings", test_vlstrings, cleanup_vlstrings, "Variable-Length Strings", NULL);
    AddTest("iterate", test_iterate, cleanup_iterate, "Group & Attribute Iteration", NULL);
    AddTest("array", test_array, cleanup_array, "Array Datatypes", NULL);
    AddTest("genprop", test_genprop, cleanup_genprop, "Generic Properties", NULL);
    AddTest("unicode", test_unicode, cleanup_unicode, "UTF-8 Encoding", NULL);
    AddTest("id", test_ids, NULL, "User-Created Identifiers", NULL);
    AddTest("misc", test_misc, cleanup_misc, "Miscellaneous", NULL);

    /* Display testing information */
    TestInfo(argv[0]);

    /* Parse command line arguments */
    TestParseCmdLine(argc, argv);

    /* Perform requested testing */
    PerformTests();

    /* Display test summary, if requested */
    if (GetTestSummary())
        TestSummary();

    /* Clean up test files, if allowed */
    if (GetTestCleanup() && !getenv(HDF5_NOCLEANUP))
        TestCleanup();

    /* Release test infrastructure */
    TestShutdown();

    /* Exit failure if errors encountered; else exit success. */
    /* No need to print anything since PerformTests() already does. */
    if (GetTestNumErrs() > 0)
        exit(EXIT_FAILURE);
    else
        exit(EXIT_SUCCESS);
} /* end main() */
