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
 *              Friday, April 28, 2006
 *
 * Purpose:     Test support stuff.
 */

#ifndef _H5HLTEST_H
#define _H5HLTEST_H

/* Get the HDF5 public header */
#include "hdf5.h"

/* Include the High-Level private header */
#include "H5HLprivate2.h"

/* These are copied from the test/h5test.h header */
#define TESTING(WHAT)	{printf("%-70s", "Testing " WHAT); fflush(stdout);}
#define TESTING2(WHAT)  {printf("%-70s", "Testing     " WHAT); fflush(stdout);}
#define TESTING3(WHAT)  {printf("%-70s", "" WHAT); fflush(stdout);}
#define PASSED()	{puts(" PASSED");fflush(stdout);}
#define H5_FAILED()	{puts("*FAILED*");fflush(stdout);}
#define SKIPPED()	{puts(" -SKIP-");fflush(stdout);}
#define EXAMPLE(WHAT)	{printf("%-70s", "Example " WHAT); fflush(stdout);}

#endif /* _H5HLTEST_H */

