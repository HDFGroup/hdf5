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
   h5cpputil.h - Header file of the utilities/misc for HDF5 C++ tests.

   EXTERNAL ROUTINES/VARIABLES:

 ***************************************************************************/

#ifndef _h5cpputil_h
#define _h5cpputil_h

#include "h5test.h"

#ifndef H5_NO_NAMESPACE
using namespace H5;
#endif

#ifndef H5_NO_STD
int test_report (int, const H5std_string&);
using std::cerr;
using std::endl;
#else
int test_report (int, const H5std_string&);
#endif

void issue_fail_msg(const char* where, int line, const char* file_name,
		    const char* message="");

template <class Type1, class Type2>
    void verify_val(Type1 x, Type2 value, const char* where, int line, const char* file_name)
{
    if (GetTestVerbosity()>=VERBO_HI)
    {
        cerr << "   Call to routine: " << where << " at line " << line
	     << " in " << file_name <<  " had value " << x << endl;
    }
    if (x != value)
    {
        cerr << "*** UNEXPECTED VALUE from " << where << " should be "
	     << value << ", but is " << x << " at line " << line
	     << " in " << file_name << endl;
	IncTestNumErrs();
    }
}

template <class Type1, class Type2>
    void verify_val(Type1 x, Type2 value, const char* msg, const char* file_name, int line)
{
    if (x != value)
    {
        cerr << "*** UNEXPECTED VALUE: " << file_name << ":line " << line
	     << ":" << msg << " different: " << x << ", should be " << value
	     << endl;
	IncTestNumErrs();
    }
}

class InvalidActionException : public Exception {
   public:
	InvalidActionException(const H5std_string func_name, const H5std_string message = DEFAULT_MSG);
	InvalidActionException();
	virtual ~InvalidActionException();
};

#endif
