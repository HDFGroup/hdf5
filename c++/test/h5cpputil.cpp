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
   util.cpp - Utility routines for HDF5 C++ tests.

   EXTERNAL ROUTINES/VARIABLES:

 ***************************************************************************/

#ifdef OLD_HEADER_FILENAME
#include <iostream.h>
#else
#include <iostream>
#endif
#include <string>

#ifndef H5_NO_STD
using namespace std;
#endif

#include "h5test.h"
#include "h5cpputil.h"

/*-------------------------------------------------------------------------
 * Function:	test_report
 *
 * Purpose:	Prints out the number of errors for the tests indicated
 * 		by 'testname,' if there were any failures occurred.  If 
 * 		no failure, test_report prints out the tests passed message.
 *
 * Return:	if any failure has occurred:	1
 *
 *		if no failure occurs:	0
 *
 * Programmer:	Binh-Minh Ribler (using C code segment for reporting tests)
 *		Friday, February 6, 2001
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int test_report( int nerrors, const string& testname )
{
   if (nerrors)
   {
      nerrors = MAX(1, nerrors);
	if (1 == nerrors)
	    cerr << "***** " << nerrors << testname
					<< " TEST FAILED! *****" << endl;
	else
	    cerr << "***** " << nerrors << testname
					<< " TESTS FAILED! *****" << endl;
      return 1;
   }
   else 
   {
      cerr << "All" << testname << " tests passed." << endl;
      return 0;
   }
}

