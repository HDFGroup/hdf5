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
   General test wrapper for HDF5 base library test programs

   DESIGN
   Each test function should be implemented as function having no
   parameters and returning void (i.e. no return value).  They should be put
   into the list of InitTest() calls in main() below.  Functions which depend
   on other functionality should be placed below the InitTest() call for the
   base functionality testing.
   Each test module should include testhdf5.h and define a unique set of
   names for test files they create.

   BUGS/LIMITATIONS

   EXPORTED ROUTINES/VARIABLES:
   Two variables are exported: num_errs, and Verbosity.

 ***************************************************************************/

#ifdef __MWERKS__
#include <console.h>
#endif  /* __MWERKS__ */

#include <stdarg.h>

#define MAXNUMOFTESTS 30
#define HDF5_TEST_MASTER

/* Internal Variables */
static int      Index = 0;

/* Global variables */
int             num_errs = 0;
int             Verbosity;

// Use C version of the header file testhdf5.h instead of re-coding it
#include "testhdf5.h"
#include <iostream>
#include "H5Cpp.h"

#ifndef H5_NO_NAMESPACE
using namespace H5;
#endif  /* !H5_NO_NAMESPACE */

struct TestStruct {
    int    NumErrors;
    char   Description[64];
    int    SkipFlag;
    char   Name[16];
    void (*Call) (void);
    void (*Cleanup) (void);
} Test[MAXNUMOFTESTS];

static void InitTest(const char *, void (*) (void), void (*) (void), const char *TheDescr);
static void usage(void);

static void InitTest(const char *TheName, void (*TheCall) (void), void (*Cleanup) (void), const char *TheDescr)
{
    if (Index >= MAXNUMOFTESTS) {
        print_func("Uh-oh, too many tests added, increase MAXNUMOFTEST!\n");
        exit(-1);
    }

    HDstrcpy(Test[Index].Description, TheDescr);
    HDstrcpy(Test[Index].Name, TheName);
    Test[Index].Call = TheCall;
    Test[Index].Cleanup = Cleanup;
    Test[Index].NumErrors = -1;
    Test[Index].SkipFlag = 0;
    Index++;
}

static void 
usage(void)
{
    print_func("Usage: testhdf5 [-v[erbose] (l[ow]|m[edium]|h[igh]|0-10)] \n");
    print_func("               [-[e]x[clude] name+] \n");
    print_func("               [-o[nly] name+] \n");
    print_func("               [-b[egin] name] \n");
    print_func("               [-s[ummary]]  \n");
    print_func("               [-c[leanoff]]  \n");
    print_func("               [-n[ocaching]]  \n");
    print_func("               [-h[elp]]  \n");
    print_func("\n\n");
    print_func("verbose   controls the amount of information displayed\n");
    print_func("exclude   to exclude tests by name\n");
    print_func("only      to name tests which should be run\n");
    print_func("begin     start at the name of the test givin\n");
    print_func("summary   prints a summary of test results at the end\n");
    print_func("cleanoff  does not delete *.hdf files after execution of tests\n");
    print_func("nocaching do not turn on low-level DD caching\n");
    print_func("help      print out this information\n");
    print_func("\n\n");
    print_func("This program currently tests the following: \n\n");
    print_func("%16s %s\n", "Name", "Description");
    print_func("%16s %s\n", "----", "-----------");

    for (int i = 0; i < Index; i++)
        print_func("%16s %s\n", Test[i].Name, Test[i].Description);

    print_func("\n\n");
}

/*
 * This routine is designed to provide equivalent functionality to 'printf'
 * and allow easy replacement for environments which don't have stdin/stdout
 * available.  (i.e. Windows & the Mac)
 */
int 
print_func(const char *format,...)
{
    va_list arglist;
    int     ret_value;

    va_start(arglist, format);
    ret_value = vprintf(format, arglist);
    va_end(arglist);
    return ret_value;
}

void
test_tbbt(void)
{
}

int 
main(int argc, char *argv[])
{
    int CLLoop;     /* Command Line Loop */
    int Loop, Loop1;
    int Summary = 0;
    int CleanUp = 1;
    int Cache = 1;

#ifdef __MWERKS__
    argc = ccommand(&argv);
#endif  /* __MWERKS__ */

#if !(defined MAC || defined __MWERKS__ || defined SYMANTEC_C)
    /* Un-buffer the stdout and stderr */
    setbuf(stderr, NULL);
    setbuf(stdout, NULL);
#endif  /* !(MAC || __MWERKS__ || SYMANTEC_C) */

    /*
     * Turn off automatic error reporting since we do it ourselves.  Besides,
     * half the functions this test calls are private, so automatic error
     * reporting wouldn't do much good since it's triggered at the API layer.
     */
    Exception::dontPrint();

    // Tests are generally arranged from least to most complexity... 
    //InitTest("metadata", test_metadata, cleanup_metadata, "Encode/decode metadata code");

    // C++ API doesn't need this test */
    InitTest("tbbt", test_tbbt, NULL,  "Threaded, Balanced, Binary Trees - not tested");

    // testing file creation and opening in tfile.cpp
    InitTest("file", test_file, cleanup_file, "Low-Level File I/O");
    InitTest("h5s",  test_h5s,  cleanup_h5s,  "Dataspaces");

    /* Comment out tests that are not done yet. - BMR, Feb 2001
    InitTest("attr", test_attr, cleanup_attr,  "Attributes");
    InitTest("select", test_select, cleanup_select,  "Selections");
    InitTest("time", test_time, cleanup_time,  "Time Datatypes");
    InitTest("reference", test_reference, cleanup_reference,  "References");
    InitTest("vltypes", test_vltypes, cleanup_vltypes,  "Variable-Length Datatypes");
    InitTest("vlstrings", test_vlstrings, cleanup_vlstrings,  "Variable-Length Strings");
    InitTest("iterate", test_iterate, cleanup_iterate,  "Group & Attribute Iteration");
    InitTest("array", test_array, cleanup_array,  "Array Datatypes");
    InitTest("genprop", test_genprop, cleanup_genprop,  "Generic Properties");
Comment out tests that are not done yet */

    Verbosity = 4;  /* Default Verbosity is Low */
    unsigned major, minor, release;
    H5Library::getLibVersion( major, minor, release);

    print_func("\nFor help use: testhdf5 -help\n");
    print_func("Linked with hdf5 version %u.%u release %u\n",
	       (unsigned)major, (unsigned)minor, (unsigned)release);

    for (CLLoop = 1; CLLoop < argc; CLLoop++) {
        if ((argc > CLLoop + 1) && ((HDstrcmp(argv[CLLoop], "-verbose") == 0) ||
                                    (HDstrcmp(argv[CLLoop], "-v") == 0))) {
            if (argv[CLLoop + 1][0] == 'l')
                Verbosity = 4;
            else if (argv[CLLoop + 1][0] == 'm')
                Verbosity = 6;
            else if (argv[CLLoop + 1][0] == 'h')
                Verbosity = 10;
            else
                Verbosity = atoi(argv[CLLoop + 1]);
        }

        if ((argc > CLLoop) && ((HDstrcmp(argv[CLLoop], "-summary") == 0) ||
                                (HDstrcmp(argv[CLLoop], "-s") == 0)))
            Summary = 1;

        if ((argc > CLLoop) && ((HDstrcmp(argv[CLLoop], "-help") == 0) ||
                                (HDstrcmp(argv[CLLoop], "-h") == 0))) {
            usage();
            exit(0);
        }

        if ((argc > CLLoop) && ((HDstrcmp(argv[CLLoop], "-cleanoff") == 0) ||
                                (HDstrcmp(argv[CLLoop], "-c") == 0)))
            CleanUp = 0;

        if ((argc > CLLoop) && ((HDstrcmp(argv[CLLoop], "-nocache") == 0) ||
                                (HDstrcmp(argv[CLLoop], "-n") == 0))) {
            Cache = 0;
	    printf ("Cache = %d\n", Cache);
	}

        if ((argc > CLLoop + 1) && ((HDstrcmp(argv[CLLoop], "-exclude") == 0) ||
                                    (HDstrcmp(argv[CLLoop], "-x") == 0))) {
            Loop = CLLoop + 1;

            while ((Loop < argc) && (argv[Loop][0] != '-')) {
                for (Loop1 = 0; Loop1 < Index; Loop1++)
                    if (HDstrcmp(argv[Loop], Test[Loop1].Name) == 0)
                        Test[Loop1].SkipFlag = 1;

                Loop++;
            }
        }

        if ((argc > CLLoop + 1) && ((HDstrcmp(argv[CLLoop], "-begin") == 0) ||
                                    (HDstrcmp(argv[CLLoop], "-b") == 0))) {
            Loop = CLLoop + 1;

            while ((Loop < argc) && (argv[Loop][0] != '-')) {
                for (Loop1 = 0; Loop1 < Index; Loop1++) {
                    if (HDstrcmp(argv[Loop], Test[Loop1].Name) != 0)
                        Test[Loop1].SkipFlag = 1;
                    if (HDstrcmp(argv[Loop], Test[Loop1].Name) == 0)
                        Loop1 = Index;
                }

                Loop++;
            }
        }

        if ((argc > CLLoop + 1) && ((HDstrcmp(argv[CLLoop], "-only") == 0) ||
                                    (HDstrcmp(argv[CLLoop], "-o") == 0))) {
            for (Loop = 0; Loop < Index; Loop++)
                Test[Loop].SkipFlag = 1;
            Loop = CLLoop + 1;
            while ((Loop < argc) && (argv[Loop][0] != '-')) {
                for (Loop1 = 0; Loop1 < Index; Loop1++)
                    if (HDstrcmp(argv[Loop], Test[Loop1].Name) == 0)
                        Test[Loop1].SkipFlag = 0;
                Loop++;
            }
        }
    }

#ifdef NOT_YET
    if (Cache)                  /* turn on caching, unless we were instucted not to */
        Hcache(CACHE_ALL_FILES, TRUE);
#endif /* NOT_YET */

    for (Loop = 0; Loop < Index; Loop++) {
        if (Test[Loop].SkipFlag) {
            MESSAGE(2, ("Skipping -- %s \n", Test[Loop].Description));
        } else {
            MESSAGE(2, ("Testing  -- %s (%s) \n", Test[Loop].Description,
                        Test[Loop].Name));
            MESSAGE(5, ("===============================================\n"));
            Test[Loop].NumErrors = num_errs;
            (*Test[Loop].Call) ();
            Test[Loop].NumErrors = num_errs - Test[Loop].NumErrors;
            MESSAGE(5, ("===============================================\n"));
            MESSAGE(5, ("There were %d errors detected.\n\n", (int) Test[Loop].NumErrors));
        }
    }

    MESSAGE(2, ("\n\n"))

    if (num_errs)
        print_func("!!! %d Error(s) were detected !!!\n\n", (int) num_errs);
    else
        print_func("All tests were successful. \n\n");

    if (Summary) {
        print_func("Summary of Test Results:\n");
        print_func("Name of Test     Errors Description of Test\n");
        print_func("---------------- ------ --------------------------------------\n");

        for (Loop = 0; Loop < Index; Loop++) {
            if (Test[Loop].NumErrors == -1)
                print_func("%16s %6s %s\n", Test[Loop].Name, "N/A", Test[Loop].Description);
            else
                print_func("%16s %6d %s\n", Test[Loop].Name, (int) Test[Loop].NumErrors,
                           Test[Loop].Description);
        }

        print_func("\n\n");
    }

    if (CleanUp && !getenv("HDF5_NOCLEANUP")) {
        MESSAGE(2, ("\nCleaning Up temp files...\n\n"));

        /* call individual cleanup routines in each source module */
        for (Loop = 0; Loop < Index; Loop++)
            if (!Test[Loop].SkipFlag && Test[Loop].Cleanup!=NULL)
                (*Test[Loop].Cleanup) ();
    }

    return num_errs;
}
