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
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Friday, November 20, 1998
 *
 * Purpose:     Test support stuff.
 */
#ifndef _H5TEST_H
#define _H5TEST_H

#undef NDEBUG
#include "hdf5.h"
#include "H5private.h"

#ifdef H5_STDC_HEADERS
#   include <signal.h>
#endif

#define H5T_PACKAGE
#include "H5Tpkg.h"		/*to turn off hardware conversions*/

/*
 * This contains the filename prefix specificied as command line option for
 * the parallel test files. 
 */
extern char *paraprefix;
#ifdef H5_HAVE_PARALLEL
extern MPI_Info h5_io_info_g;         /* MPI INFO object for IO */
#endif

/*
 * The name of the test is printed by saying TESTING("something") which will
 * result in the string `Testing something' being flushed to standard output.
 * If a test passes, fails, or is skipped then the PASSED(), H5_FAILED(), or
 * SKIPPED() macro should be called.  After H5_FAILED() or SKIPPED() the caller
 * should print additional information to stdout indented by at least four
 * spaces.  If the h5_errors() is used for automatic error handling then
 * the H5_FAILED() macro is invoked automatically when an API function fails.
 */
#define TESTING(WHAT)	{printf("Testing %-62s",WHAT); fflush(stdout);}
#define PASSED()	{puts(" PASSED");fflush(stdout);}
#define H5_FAILED()	{puts("*FAILED*");fflush(stdout);}
#define SKIPPED()	{puts(" -SKIP-");fflush(stdout);}

/*
 * Print the current location on the standard output stream.
 */
#define AT() 		printf ("	 at %s:%d in %s()...\n",	      \
				__FILE__, __LINE__, __FUNCTION__);


#ifdef __cplusplus
extern "C" {
#endif

int h5_cleanup(const char *base_name[], hid_t fapl);
herr_t h5_errors(void *client_data);
char *h5_fixname(const char *base_name, hid_t fapl, char *fullname,
		 size_t size);
hid_t h5_fileaccess(void);
void h5_no_hwconv(void);
void h5_reset(void);
void h5_show_hostname(void);
#ifdef H5_HAVE_PARALLEL
int h5_set_info_object(void);
void h5_dump_info_object(MPI_Info info);
#endif

#ifdef __cplusplus
}
#endif
#endif
