/*
 * Copyright © 1998 NCSA
 *                  All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Friday, November 20, 1998
 *
 * Purpose:     Test support stuff.
 */
#ifndef _H5TEST_H
#define _H5TEST_H

#undef NDEBUG
#include <hdf5.h>

#include <H5private.h>
#ifdef STDC_HEADERS
#   include <signal.h>
#endif

#define H5T_PACKAGE
#include <H5Tpkg.h>		/*to turn off hardware conversions*/


/*
 * This array should contain a list of file base names created by the test.
 * The base name is a word like `test' which will have a prefix and suffix
 * added to result in something like `ufs:/u/matzke/test.h5'
 */
extern const char *FILENAME[];

/*
 * This contains the filename prefix specificied as command line option for
 * the parallel test files. 
 */
extern char *paraprefix;

/*
 * The name of the test is printed by saying TESTING("something") which will
 * result in the string `Testing something' being flushed to standard output.
 * If a test passes, fails, or is skipped then the PASSED(), FAILED(), or
 * SKIPPED() macro should be called.  After FAILED() or SKIPPED() the caller
 * should print additional information to stdout indented by at least four
 * spaces.  If the h5_errors() is used for automatic error handling then
 * the FAILED() macro is invoked automatically when an API function fails.
 */
#define TESTING(WHAT)	{printf("%-70s", "Testing " WHAT); fflush(stdout);}
#define PASSED()	{puts(" PASSED");fflush(stdout);}
#define FAILED()	{puts("*FAILED*");fflush(stdout);}
#define SKIPPED()	{puts(" -SKIP-");fflush(stdout);}

/*
 * Print the current location on the standard output stream.
 */
#define AT() 		printf ("	 at %s:%d in %s()...\n",	      \
				__FILE__, __LINE__, __FUNCTION__);


#ifdef __cplusplus
extern "C" {
#endif

int h5_cleanup(hid_t fapl);
herr_t h5_errors(void *client_data);
char *h5_fixname(const char *base_name, hid_t fapl, char *fullname,
		 size_t size);
hid_t h5_fileaccess(void);
void h5_no_hwconv(void);
void h5_reset(void);

#ifdef __cplusplus
}
#endif
#endif
