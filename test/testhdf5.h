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

/* Include required headers */
#include "hdf5.h"
#include <stdarg.h>

/* Define these for use in all the tests */
#ifndef HDF5_TEST_MASTER
extern
#endif
int         num_errs
#ifdef HDF5_TEST_MASTER
= 0
#endif
,           Verbosity
#ifdef HDF5_TEST_MASTER
= 0
#endif
           ;

/* Use %ld to print the value because long should cover most cases. */
/* Used to make certain a return value _is_not_ a value */
#define CHECK(ret, val, where) \
do {if (Verbosity>9) print_func("   Call to routine: %15s at line %4d in %s returned %ld \n",where,(int)__LINE__,__FILE__,(long)ret);\
if(ret == val) {print_func("*** UNEXPECTED RETURN from %s is %ld at line %4d in %s\n", where, (long)ret, (int)__LINE__,__FILE__); num_errs++;} \
} while(0)

#define CHECK_I(ret,where) {						      \
   if (Verbosity>9) {							      \
      print_func("   Call to routine: %15s at line %4d in %s returned %ld\n", \
		 (where), (int)__LINE__, __FILE__, (long)(ret));	      \
   }									      \
   if ((ret)<0) {							      \
      print_func ("*** UNEXPECTED RETURN from %s is %ld line %4d in %s\n",    \
		  (where), (long)(ret), (int)__LINE__, __FILE__);	      \
      num_errs++;							      \
   }									      \
}
		     
#define CHECK_PTR(ret,where) {						      \
   if (Verbosity>9) {							      \
      print_func("   Call to routine: %15s at line %4d in %s returned %p\n",  \
		 (where), (int)__LINE__, __FILE__, (ret));		      \
   }									      \
   if (!(ret)) {							      \
      print_func ("*** UNEXPECTED RETURN from %s is NULL line %4d in %s\n",   \
		  (where), (int)__LINE__, __FILE__);			      \
      num_errs++;							      \
   }									      \
}

/* Used to make certain a return value _is_ a value */
#define VERIFY(x, val, where) \
do {if (Verbosity>9) print_func("   Call to routine: %15s at line %4d in %s had value %ld \n",where,(int)__LINE__,__FILE__,(long)x);\
if(x != val) {print_func("*** UNEXPECTED VALUE from %s is %ld at line %4d in %s\n", where, (long)x,(int)__LINE__,__FILE__); num_errs++;} \
} while(0)

/* Used to document process through a test and to check for errors */
#define RESULT(ret,func) \
do { \
if (Verbosity>8) print_func("   Call to routine: %15s at line %4d in %s returned %ld \n",func,(int)__LINE__,__FILE__,(long)ret); \
if (Verbosity>9) HEprint(stdout,0); \
if(ret == FAIL) {print_func("*** UNEXPECTED RETURN from %s is %ld at line %4d in %s\n", func, (long)ret,(int)__LINE__,__FILE__); num_errs++;} \
} while(0)

/* Used to document process through a test */
#define MESSAGE(v,a) {if (Verbosity>v) {a}}

/* definitions for command strings */
#define VERBOSITY_STR	"Verbosity"
#define SKIP_STR	"Skip"
#define TEST_STR	"Test"
#define CLEAN_STR	"Cleanup"

/* System command to use for Cleanup */
#ifdef VMS
#define CLEAN_CMD	"delete *.hdf;*"
#else
#   ifdef WIN32
#   define CLEAN_CMD  "del *.hdf"   
#   else
/* default is Unix */
#   define CLEAN_CMD	"rm -f *.hdf"
#   endif  /* WIN32  */
#endif /*VMS */

/* Prototypes for the support routines */
int print_func(const char *, ...);

/* Prototypes for the test routines */
void test_metadata(void);
void test_file(void);
void test_heap (void);
void test_ohdr (void);
void test_stab (void);

#endif /* HDF5TEST_H */

