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

/* $Id$ */

/*
 * This header file contains information required for testing the HDF5 library.
 */

#ifndef HDF5TEST_H
#define HDF5TEST_H

/*
 * Include required headers.  This file tests internal library functions,
 * so we include the private headers here.
 */
#include <H5private.h>
#include <H5Eprivate.h>

extern int              num_errs;
extern int              Verbosity;

/* Use %ld to print the value because long should cover most cases. */
/* Used to make certain a return value _is_not_ a value */
#define CHECK(ret, val, where) do {					      \
    if (Verbosity>9) print_func("   Call to routine: %15s at line %4d "	      \
				"in %s returned %ld \n",		      \
				where, (int)__LINE__, __FILE__,		      \
				(long)ret);				      \
    if (ret == val) {							      \
	print_func("*** UNEXPECTED RETURN from %s is %ld at line %4d "	      \
		   "in %s\n", where, (long)ret, (int)__LINE__, __FILE__);     \
	num_errs++;							      \
	H5Eprint (stdout);						      \
    }									      \
    H5Eclear();								      \
} while(0)

#define CHECK_I(ret,where) {						      \
   if (Verbosity>9) {							      \
      print_func("   Call to routine: %15s at line %4d in %s returned %ld\n", \
                 (where), (int)__LINE__, __FILE__, (long)(ret));	      \
   }									      \
   if ((ret)<0) {							      \
      print_func ("*** UNEXPECTED RETURN from %s is %ld line %4d in %s\n",    \
                  (where), (long)(ret), (int)__LINE__, __FILE__);	      \
      H5Eprint (stdout);						      \
      num_errs++;							      \
   }									      \
   H5Eclear ();								      \
}

#define CHECK_PTR(ret,where) {						      \
   if (Verbosity>9) {							      \
      print_func("   Call to routine: %15s at line %4d in %s returned %p\n",  \
                 (where), (int)__LINE__, __FILE__, (ret));		      \
   }									      \
   if (!(ret)) {							      \
      print_func ("*** UNEXPECTED RETURN from %s is NULL line %4d in %s\n",   \
                  (where), (int)__LINE__, __FILE__);			      \
      H5Eprint (stdout);						      \
      num_errs++;							      \
   }									      \
   H5Eclear ();								      \
}

/* Used to make certain a return value _is_ a value */
#define VERIFY(x, val, where) do {					      \
    if (Verbosity>9) {							      \
	print_func("   Call to routine: %15s at line %4d in %s had value "    \
		   "%ld \n", where, (int)__LINE__, __FILE__, (long)x);	      \
    }									      \
    if (x != val) {							      \
	print_func("*** UNEXPECTED VALUE from %s is %ld at line %4d "	      \
		   "in %s\n", where, (long)x, (int)__LINE__, __FILE__);	      \
	H5Eprint (stdout);						      \
	num_errs++;							      \
    }									      \
    H5Eclear();								      \
} while(0)

/* Used to document process through a test and to check for errors */
#define RESULT(ret,func) do {						      \
    if (Verbosity>8) {							      \
	print_func("   Call to routine: %15s at line %4d in %s returned "     \
		   "%ld\n", func, (int)__LINE__, __FILE__, (long)ret);	      \
    }									      \
    if (Verbosity>9) HEprint(stdout, 0);				      \
    if (ret == FAIL) {							      \
	print_func("*** UNEXPECTED RETURN from %s is %ld at line %4d "	      \
		   "in %s\n", func, (long)ret, (int)__LINE__, __FILE__);      \
	H5Eprint (stdout);						      \
	num_errs++;							      \
    }									      \
    H5Eclear();								      \
} while(0)

/* Used to document process through a test */
#define MESSAGE(V,A) {if (Verbosity>(V)) print_func A;}

/* definitions for command strings */
#define VERBOSITY_STR   "Verbosity"
#define SKIP_STR        "Skip"
#define TEST_STR        "Test"
#define CLEAN_STR       "Cleanup"

/* Prototypes for the support routines */
int                     print_func(const char *,...);

/* Prototypes for the test routines */
void                    test_metadata(void);
void                    test_file(void);
void                    test_heap(void);
void                    test_ohdr(void);
void                    test_stab(void);
void                    test_h5t(void);
void                    test_h5s(void);
void                    test_h5d(void);
void                    test_attr(void);
void                    test_select(void);
void                    test_reference(void);

/* Prototypes for the cleanup routines */
void                    cleanup_metadata(void);
void                    cleanup_file(void);
void                    cleanup_heap(void);
void                    cleanup_ohdr(void);
void                    cleanup_stab(void);
void                    cleanup_h5s(void);
void                    cleanup_attr(void);
void                    cleanup_select(void);
void                    cleanup_reference(void);

#endif /* HDF5cleanup_H */
