// C++ informative line for the emacs editor: -*- C++ -*-
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

#ifndef _H5RefCounter_H
#define _H5RefCounter_H

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

// define bool type for platforms that don't support bool yet
// Note: it is added here because most of the C++ library source
// files include this header file
#ifdef BOOL_NOTDEFINED
#ifdef false
#undef false
#endif
#ifdef true
#undef true
#endif
typedef int bool;
const bool  false = 0;
const bool  true  = 1;

#endif

class H5_DLLCPP RefCounter {
   public:
	// Creates a reference counter to be used by an HDF5 object
	RefCounter();

        int getCounter () const;
        void increment();
        void decrement();

	// this bool function is used to determine whether to close an
	// HDF5 object when there are no more reference to that object
	bool noReference();

	~RefCounter();

   private:
	int counter; // keeps track of number of copies of an object
};
#ifndef H5_NO_NAMESPACE
}
#endif
#endif
