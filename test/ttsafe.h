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

/* $Id$ */

/*
 * This header file contains information required for testing the HDF5 library.
 */

#ifndef HDF5TEST_H
#define HDF5TEST_H

#include "hdf5.h"
#include <string.h>

/*
 * Include required headers.  This file tests internal library functions,
 * so we include the private headers here.
 */
#include "H5private.h"
#include "H5Eprivate.h"

#ifdef H5_HAVE_THREADSAFE
/* Include pthread library for threadsafe tests */
#include <pthread.h>

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
extern char*            gen_name(int);
extern int              num_digits(int);

/* Prototypes for the test routines */
void                    tts_dcreate(void);
void                    tts_error(void);
void                    tts_cancel(void);
void                    tts_acreate(void);

/* Prototypes for the cleanup routines */
void                    cleanup_dcreate(void);
void                    cleanup_error(void);
void                    cleanup_cancel(void);
void                    cleanup_acreate(void);

#endif /* H5_HAVE_THREADSAFE */
#endif /* HDF5_TESTH */
