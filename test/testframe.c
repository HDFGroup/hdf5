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
 * Programmer:  Quincey Koziol <koziol@ncsa.uiuc.edu>
 *              Tuesday, January  6, 2004
 *
 * Purpose:	Provides support functions for the testing framework.
 *		
 */

#include "testhdf5.h"

/*
 * Definitions for the testing structure.
 */
#define MAXNUMOFTESTS   30
#define MAXTESTNAME     16
#define MAXTESTDESC     64

typedef struct TestStruct {
	int    NumErrors;
	char   Description[MAXTESTDESC];
	int    SkipFlag;
	char   Name[MAXTESTNAME];
	void (*Call)(void);
	void (*Cleanup)(void);
	const void *Parameters;
} TestStruct;


/*
 * Variables used by testing framework.
 */
static int num_errs = 0;        /* Total number of errors during testing */
static int Verbosity = VERBO_DEF;       /* Default Verbosity is Low */
static TestStruct Test[MAXNUMOFTESTS];
static int    Index = 0;
static const void *Test_parameters = NULL;


/*
 * Setup a test function and add it to the list of tests.
 *      It must have no parameters and returns void.
 * TheName--short test name.
 *    If the name starts with '-', do not run it by default.
 * TheCall--the test routine.
 * Cleanup--the cleanup routine for the test.
 * TheDescr--Long description of the test.
 * Parameters--pointer to extra parameters. Use NULL if none used.
 *    Since only the pointer is copied, the contents should not change.
 */
void 
AddTest(const char *TheName, void (*TheCall) (void), void (*Cleanup) (void), const char *TheDescr, const void *Parameters)
{
    /* Sanity checking */
    if (Index >= MAXNUMOFTESTS) {
        printf("Too many tests added, increase MAXNUMOFTEST(%d).\n",
		MAXNUMOFTESTS);
        exit(-1);
    }                           /* end if */
    if (HDstrlen(TheDescr) >= MAXTESTDESC) {
        printf("Test description too long, increase MAXTESTDESC(%d).\n",
		MAXTESTDESC);
        exit(-1);
    } /* end if */
    if (HDstrlen(TheName) >= MAXTESTNAME) {
        printf("Test name too long, increase MAXTESTNAME(%d).\n",
		MAXTESTNAME);
        exit(-1);
    } /* end if */

    /* Set up test function */
    HDstrcpy(Test[Index].Description, TheDescr);
    if (*TheName != '-'){
	HDstrcpy(Test[Index].Name, TheName);
	Test[Index].SkipFlag = 0;
    }
    else {	/* skip test by default */
	HDstrcpy(Test[Index].Name, TheName+1);
	Test[Index].SkipFlag = 1;
    }
    Test[Index].Call = TheCall;
    Test[Index].Cleanup = Cleanup;
    Test[Index].NumErrors = -1;
    Test[Index].Parameters = Parameters;

    /* Increment test count */
    Index++;
}


/*
 * Initialize testing framework
 */
void TestInit(void)
{
#if !(defined MAC || defined __MWERKS__ || defined SYMANTEC_C)
    /* Un-buffer the stdout and stderr */
    setbuf(stderr, NULL);
    setbuf(stdout, NULL);
#endif

    /*
     * Turn off automatic error reporting since we do it ourselves.  Besides,
     * half the functions this test calls are private, so automatic error
     * reporting wouldn't do much good since it's triggered at the API layer.
     */
#ifdef H5_WANT_H5_V1_6_COMPAT
    H5Eset_auto (NULL, NULL);
#else
    H5Eset_auto (H5E_DEFAULT, NULL, NULL);
#endif /* H5_WANT_H5_V1_6_COMPAT */

}


/*
 * Print test usage.
 */
void TestUsage(void)
{
	int i;

	print_func("Usage: ttsafe [-v[erbose] (l[ow]|m[edium]|h[igh]|0-9)] \n");
	print_func("              [-[e]x[clude] name+] \n");
	print_func("              [-o[nly] name+] \n");
	print_func("              [-b[egin] name] \n");
	print_func("              [-s[ummary]]  \n");
	print_func("              [-c[leanoff]]  \n");
	print_func("              [-h[elp]]  \n");
	print_func("\n\n");
	print_func("verbose   controls the amount of information displayed\n");
	print_func("exclude   to exclude tests by name\n");
	print_func("only      to name tests which should be run\n");
	print_func("begin     start at the name of the test givin\n");
	print_func("summary   prints a summary of test results at the end\n");
	print_func("cleanoff  does not delete *.hdf files after execution of tests\n");
	print_func("help      print out this information\n");
	print_func("\n\n");
	print_func("This program currently tests the following: \n\n");
	print_func("%16s %s\n", "Name", "Description");
	print_func("%16s %s\n", "----", "-----------");

	for (i = 0; i < Index; i++)
		print_func("%16s %s\n", Test[i].Name, Test[i].Description);

	print_func("\n\n");
}


/*
 * Print test info.
 */
void TestInfo(const char *ProgName)
{
    unsigned major, minor, release;

    H5get_libversion(&major, &minor, &release);

    print_func("\nFor help use: %s -help\n",ProgName);
    print_func("Linked with hdf5 version %u.%u release %u\n", major, minor, release);
}


/*
 * Parse command line information.
 *      argc, argv: the usual command line argument count and strings
 *      Summary:    Return if summary is desired. Default no.
 *      CleanUp:    Return if Cleanup is desired. Default yes.
 *      extra_parse: Extra Parse function provided by individual application.
 *      	    NULL means no extra parsing needed.
 *
 * Modification:
 * 	2004/08/12 Albert Cheng.  Add extra_parse feature.
 */
void TestParseCmdLine(int argc, char *argv[], int *Summary, int *CleanUp, int (*extra_parse)(int ac, char *av[]))
{
    while (argv++, --argc > 0){
	if ((HDstrcmp(*argv, "-verbose") == 0) ||
				(HDstrcmp(*argv, "-v") == 0)) {
	    if (argc > 0){
		--argc; ++argv;
		ParseTestVerbosity(*argv);
	    }else{
		TestUsage();
		exit(1);
	    }
	}
	else if (((HDstrcmp(*argv, "-exclude") == 0) ||
				    (HDstrcmp(*argv, "-x") == 0))) {
	    if (argc > 0){
		--argc; ++argv;
		SetTest(*argv, SKIPTEST);
	    }else{
		TestUsage();
		exit(1);
	    }
	}
	else if (((HDstrcmp(*argv, "-begin") == 0) ||
				    (HDstrcmp(*argv, "-b") == 0))) {
	    if (argc > 0){
		--argc; ++argv;
		SetTest(*argv, BEGINTEST);
	    }else{
		TestUsage();
		exit(1);
	    }
	}
	else if (((HDstrcmp(*argv, "-only") == 0) ||
				    (HDstrcmp(*argv, "-o") == 0))) {
	    if (argc > 0){
		int Loop;
		--argc; ++argv;
		/* Skip all tests, then activate only one. */
		for (Loop = 0; Loop < Index; Loop++)
		    Test[Loop].SkipFlag = 1;
		SetTest(*argv, ONLYTEST);
	    }else{
		TestUsage();
		exit(1);
	    }
	}
	else if ((HDstrcmp(*argv, "-summary") == 0) || (HDstrcmp(*argv, "-s") == 0))
            *Summary = 1;
	else if ((HDstrcmp(*argv, "-help") == 0) || (HDstrcmp(*argv, "-h") == 0)) {
            TestUsage();
            exit(0);
        }
	else if ((HDstrcmp(*argv, "-cleanoff") == 0) || (HDstrcmp(*argv, "-c") == 0))
            *CleanUp = 0;
	else {
	    /* non-standard option.  Break out. */
	    break;
	}

    }

    /* Call extra parsing function if provided. */
    if (NULL != extra_parse){
	extra_parse(argc+1, argv-1);
    }
}


/*
 * Perform Tests.
 */
void PerformTests(void)
{
    int                     Loop;

    for (Loop = 0; Loop < Index; Loop++)
        if (Test[Loop].SkipFlag) {
            MESSAGE(2, ("Skipping -- %s (%s) \n", Test[Loop].Description, Test[Loop].Name));
        } else {
            MESSAGE(2, ("Testing  -- %s (%s) \n", Test[Loop].Description, Test[Loop].Name));
            MESSAGE(5, ("===============================================\n"));
            Test[Loop].NumErrors = num_errs;
	    Test_parameters = Test[Loop].Parameters;
            Test[Loop].Call();
            Test[Loop].NumErrors = num_errs - Test[Loop].NumErrors;
            MESSAGE(5, ("===============================================\n"));
            MESSAGE(5, ("There were %d errors detected.\n\n", (int)Test[Loop].NumErrors));
        }

    Test_parameters = NULL;     /* clear it. */
    MESSAGE(2, ("\n\n"))

    if (num_errs)
        print_func("!!! %d Error(s) were detected !!!\n\n", (int) num_errs);
    else
        print_func("All tests were successful. \n\n");
}


/*
 * Display test summary.
 */
void TestSummary(void)
{
    int                     Loop;

    print_func("Summary of Test Results:\n");
    print_func("Name of Test     Errors Description of Test\n");
    print_func("---------------- ------ --------------------------------------\n");

    for (Loop = 0; Loop < Index; Loop++) {
        if (Test[Loop].NumErrors == -1)
            print_func("%16s %6s %s\n", Test[Loop].Name, "N/A", Test[Loop].Description);
        else
            print_func("%16s %6d %s\n", Test[Loop].Name, (int)Test[Loop].NumErrors, Test[Loop].Description);
    }

    print_func("\n\n");
}


/*
 * Cleanup files from testing
 */
void TestCleanup(void)
{
    int                     Loop;

    MESSAGE(2, ("\nCleaning Up temp files...\n\n"));

    /* call individual cleanup routines in each source module */
    for (Loop = 0; Loop < Index; Loop++)
        if (!Test[Loop].SkipFlag && Test[Loop].Cleanup!=NULL)
            Test[Loop].Cleanup();
}


/*
 * Retrieve the verbosity level for the testing framework
 */
int GetTestVerbosity(void)
{
    return(Verbosity);
}

/*
 * Set the verbosity level for the testing framework.
 * Return previous verbosity level.
 */
int SetTestVerbosity(int newval)
{
    int oldval;

    oldval = Verbosity;
    Verbosity = newval;
    return(oldval);
}

/*
 * Parse an argument string for verbosity level and set it.
 */
void ParseTestVerbosity(char *argv)
{
    if (*argv == 'l')
	SetTestVerbosity(VERBO_LO);
    else if (*argv == 'm')
	SetTestVerbosity(VERBO_MED);
    else if (*argv == 'h')
	SetTestVerbosity(VERBO_HI);
    else
	SetTestVerbosity(atoi(argv));
}


/*
 * Retrieve the number of testing errors for the testing framework
 */
int GetTestNumErrs(void)
{
    return(num_errs);
}


/*
 * Retrieve the current Test Parameters pointer.
 */
const void *GetTestParameters(void)
{
    return(Test_parameters);
}


/*
 * This routine is designed to provide equivalent functionality to 'printf'
 * and also increment the error count for the testing framework.
 */
int 
TestErrPrintf(const char *format, ...)
{
    va_list arglist;
    int ret_value;

    /* Increment the error count */
    num_errs++;

    /* Print the requested information */
    va_start(arglist, format);
    ret_value = vprintf(format, arglist);
    va_end(arglist);

    /* Return the length of the string produced (like printf() does) */
    return ret_value;
}


/* 
 * Set (control) which test will be tested.
 * SKIPTEST: skip this test
 * ONLYTEST: do only this test
 * BEGINETEST: skip all tests before this test
 *
 */
void SetTest(const char *testname, int action)
{
    int Loop;
    switch (action){
	case SKIPTEST:
	    for (Loop = 0; Loop < Index; Loop++)
		if (HDstrcmp(testname, Test[Loop].Name) == 0){
		    Test[Loop].SkipFlag = 1;
		    break;
		}
	    break;
	case BEGINTEST:
	    for (Loop = 0; Loop < Index; Loop++) {
		if (HDstrcmp(testname, Test[Loop].Name) != 0)
		    Test[Loop].SkipFlag = 1;
		else{
		    /* Found it. Set it to run.  Done. */
		    Test[Loop].SkipFlag = 0;
		    break;
		}
	    }
	    break;
	case ONLYTEST:
	    for (Loop = 0; Loop < Index; Loop++) {
		if (HDstrcmp(testname, Test[Loop].Name) != 0)
		    Test[Loop].SkipFlag = 1;
		else {
		    /* Found it. Set it to run. Break to skip the rest. */
		    Test[Loop].SkipFlag = 0;
		    break;
		}
	    }
	    /* skip the rest */
	    while (++Loop < Index)
		Test[Loop].SkipFlag = 1;
	    break;
	default:
	    /* error */
	    printf("*** ERROR: Unknown action (%d) for SetTest\n", action);
	    break;
    }
}
