/****************************************************************************
 * NCSA HDF                                                                 *
 * Software Development Group                                               *
 * National Center for Supercomputing Applications                          *
 * University of Illinois at Urbana-Champaign                               *
 * 605 E. Springfield, Champaign IL 61820                                   *
 *                                                                          *
 * For conditions of distribution and use, see the accompanying             *
 * hdf/COPYING file.                                                        *
 *                                                                          *
 ****************************************************************************/

#ifdef RCSID
static char             RcsId[] = "@(#)$Revision$";
#endif

/* $Id$ */

/*
 * FILE
 * ttsafe.c - HDF5 threadsafe testing framework main file.
 *
 * REMARKS
 * General test wrapper for HDF5 library thread safety test programs
 *
 * DESIGN
 * Each test function should be implemented as function having no
 * parameters and returning void (i.e. no return value).  They should be put
 * into the list of InitTest() calls in main() below.  Functions which depend
 * on other functionality should be placed below the InitTest() call for the
 * base functionality testing.
 * Each test module should include ttsafe.h and define a unique set of
 * names for test files they create.
 *
 * BUGS/LIMITATIONS
 *
 * EXPORTED ROUTINES/VARIABLES:
 * Two variables are exported: num_errs, and Verbosity.
 */

#if defined __MWERKS__
#include <console.h>
#endif

#include <stdarg.h>

#include <ttsafe.h>

#ifndef H5_HAVE_THREADSAFE
int main(void)
{
    printf("Test skipped because THREADSAFE not enabled\n");
    return 0;
}
#else

#define MAXNUMOFTESTS 50
#define HDF5_TEST_MASTER

#define MAX_NUM_NAME 1000
#define NAME_OFFSET 6         /* offset for "name<num>" */

/* Internal Variables */
static int Index = 0;

/* Global variables */
int num_errs = 0;
int Verbosity;

/* ANY new test needs to have a prototype in tproto.h */
struct TestStruct {
	int    NumErrors;
	char   Description[64];
	int    SkipFlag;
	char   Name[16];
	void (*Call)(void);
	void (*Cleanup)(void);
} Test[MAXNUMOFTESTS];

static void InitTest(const char *TheName, void (*TheCall) (void),
		     void (*Cleanup) (void), const char *TheDescr);
static void usage(void);

static void InitTest(const char *TheName, void (*TheCall) (void),
		     void (*Cleanup) (void), const char *TheDescr)
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

static void usage(void)
{
	intn i;

	print_func("Usage: ttsafe [-v[erbose] (l[ow]|m[edium]|h[igh]|0-10)] \n");
	print_func("              [-[e]x[clude] name+] \n");
	print_func("              [-o[nly] name+] \n");
	print_func("              [-b[egin] name] \n");
	print_func("              [-s[ummary]]  \n");
	print_func("              [-c[leanoff]]  \n");
	print_func("              [-n[ocaching]]  \n");
	print_func("              [-h[elp]]  \n");
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

	for (i = 0; i < Index; i++)
		print_func("%16s %s\n", Test[i].Name, Test[i].Description);

	print_func("\n\n");
}

/*
 * This routine is designed to provide equivalent functionality to 'printf'
 * and allow easy replacement for environments which don't have stdin/stdout
 * available. (i.e. Windows & the Mac)
 */
int 
print_func(const char *format, ...)
{
	va_list arglist;
	int ret_value;

	va_start(arglist, format);
	ret_value = vprintf(format, arglist);
	va_end(arglist);
	return ret_value;
}

char *gen_name(int value)
{
	char *temp;
	int i, length;

	length = num_digits(MAX_NUM_NAME - 1);
	temp = (char *)malloc((NAME_OFFSET + length + 1) * sizeof(char));
	temp = strcpy(temp, "attrib");
	temp[NAME_OFFSET + length] = '\0';

	for (i = length - 1; i >= 0; i--) {
		temp[NAME_OFFSET + i] = (char)((int)'0' + value % 10);
		value = value / 10;
	}

	return temp;
}

/* pre-condition: num must be a non-negative number */
int num_digits(int num)
{
	int i;

	if (num == 0)
		return 1;

	for (i = 0; num > 0; i++)
		num = num / 10;

	return i;
}

int main(int argc, char *argv[])
{
	int CLLoop;     /* Command Line Loop */
	int Loop, Loop1, Summary = 0, CleanUp = 1, Cache = 1;
	uintn major, minor, release;

#if defined __MWERKS__
	argc = ccommand(&argv);
#endif

#if !(defined MAC || defined __MWERKS__ || defined SYMANTEC_C)
	/* Un-buffer the stdout and stderr */
	setbuf(stderr, NULL);
	setbuf(stdout, NULL);
#endif

	/*
	 * Turn off automatic error reporting since we do it ourselves.
	 * Besides, half the functions this test calls are private, so
	 * automatic error reporting wouldn't do much good since it's
	 * triggered at the API layer.
	 */
	H5Eset_auto (NULL, NULL);

	/* Tests are generally arranged from least to most complexity... */
	InitTest("dcreate", tts_dcreate, cleanup_dcreate,
		 "multi-dataset creation");
	InitTest("error", tts_error, cleanup_error,
		 "per-thread error stacks");
	InitTest("cancel", tts_cancel, cleanup_cancel,
		 "thread cancellation safety test");
	InitTest("acreate", tts_acreate, cleanup_acreate,
		 "multi-attribute creation");

	Verbosity = 4;	/* Default Verbosity is Low */
	H5get_libversion(&major, &minor, &release);

	print_func("\nFor help use: ttsafe -help\n");
	print_func("Linked with hdf5 version %u.%u release %u\n",
		   (unsigned)major, (unsigned)minor, (unsigned)release);

	for (CLLoop = 1; CLLoop < argc; CLLoop++) {
		if (argc > CLLoop + 1 &&
			(HDstrcmp(argv[CLLoop], "-verbose") == 0 ||
			 HDstrcmp(argv[CLLoop], "-v") == 0)) {
			if (argv[CLLoop + 1][0] == 'l')
				Verbosity = 4;
			else if (argv[CLLoop + 1][0] == 'm')
				Verbosity = 6;
			else if (argv[CLLoop + 1][0] == 'h')
				Verbosity = 10;
			else
				Verbosity = atoi(argv[CLLoop + 1]);
		}                       /* end if */

		if (argc > CLLoop &&
			(HDstrcmp(argv[CLLoop], "-summary") == 0 ||
			 HDstrcmp(argv[CLLoop], "-s") == 0))
			Summary = 1;

		if (argc > CLLoop &&
			(HDstrcmp(argv[CLLoop], "-help") == 0 ||
			 HDstrcmp(argv[CLLoop], "-h") == 0)) {
			usage();
			exit(0);
		}

		if (argc > CLLoop &&
			(HDstrcmp(argv[CLLoop], "-cleanoff") == 0 ||
			 HDstrcmp(argv[CLLoop], "-c") == 0))
			CleanUp = 0;

		if (argc > CLLoop &&
			(HDstrcmp(argv[CLLoop], "-nocache") == 0 ||
			 HDstrcmp(argv[CLLoop], "-n") == 0)) {
			Cache = 0;
			print_func("Cache = %d\n", Cache);
		}

		if (argc > CLLoop + 1 &&
			(HDstrcmp(argv[CLLoop], "-exclude") == 0 ||
			 HDstrcmp(argv[CLLoop], "-x") == 0))
			for (Loop = CLLoop + 1; Loop < argc && argv[Loop][0] != '-'; Loop++)
				for (Loop1 = 0; Loop1 < Index; Loop1++)
					if (HDstrcmp(argv[Loop], Test[Loop1].Name) == 0)
						Test[Loop1].SkipFlag = 1;

		if (argc > CLLoop + 1 &&
			(HDstrcmp(argv[CLLoop], "-begin") == 0 ||
			 HDstrcmp(argv[CLLoop], "-b") == 0))
			for (Loop = CLLoop + 1; Loop < argc && argv[Loop][0] != '-'; Loop++)
				for (Loop1 = 0; Loop1 < Index; Loop1++) {
					if (HDstrcmp(argv[Loop], Test[Loop1].Name) != 0)
						Test[Loop1].SkipFlag = 1;

					if (HDstrcmp(argv[Loop], Test[Loop1].Name) == 0)
						Loop1 = Index;
				}

		if (argc > CLLoop + 1 &&
			(HDstrcmp(argv[CLLoop], "-only") == 0 ||
			 HDstrcmp(argv[CLLoop], "-o") == 0)) {
			for (Loop = 0; Loop < Index; Loop++)
				Test[Loop].SkipFlag = 1;

			for (Loop = CLLoop + 1; Loop < argc && argv[Loop][0] != '-'; Loop++)
				for (Loop1 = 0; Loop1 < Index; Loop1++)
					if (HDstrcmp(argv[Loop], Test[Loop1].Name) == 0)
						Test[Loop1].SkipFlag = 0;
		}
	} /* end for */

#ifdef NOT_YET
	if (Cache)
		/* turn on caching, unless we were instucted not to */
		Hcache(CACHE_ALL_FILES, TRUE);
#endif /* NOT_YET */

	for (Loop = 0; Loop < Index; Loop++)
		if (Test[Loop].SkipFlag) {
			MESSAGE(2, ("Skipping -- %s \n", Test[Loop].Description));
		} else {
			MESSAGE(2, ("Testing  -- %s (%s) \n", Test[Loop].Description,
				Test[Loop].Name));
			MESSAGE(5, ("===============================================\n"));
			Test[Loop].NumErrors = num_errs;
			Test[Loop].Call();
			Test[Loop].NumErrors = num_errs - Test[Loop].NumErrors;
			MESSAGE(5, ("===============================================\n"));
			MESSAGE(5, ("There were %d errors detected.\n\n",
				(int)Test[Loop].NumErrors));
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
				print_func("%16s %6s %s\n", Test[Loop].Name,
					   "N/A", Test[Loop].Description);
			else
				print_func("%16s %6d %s\n", Test[Loop].Name,
					   (int)Test[Loop].NumErrors,
					   Test[Loop].Description);
		}

		print_func("\n\n");
	}

	if (CleanUp && !getenv("HDF5_NOCLEANUP")) {
		MESSAGE(2, ("\nCleaning Up temp files...\n\n"));

		/* call individual cleanup routines in each source module */
		for (Loop = 0; Loop < Index; Loop++)
			if (!Test[Loop].SkipFlag && Test[Loop].Cleanup!=NULL)
				Test[Loop].Cleanup();
	}

	return num_errs;
} /* end main() */
#endif  /*H5_HAVE_THREADSAFE*/
