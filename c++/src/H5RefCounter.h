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

///\remarks	The features provided by this class are now handled at 
///		the C library layer; thus, the class will be removed from 
///		future releases.

class H5_DLLCPP RefCounter {
   public:
#ifndef DOXYGEN_SHOULD_SKIP_THIS
	// Returns the current value of the reference counter.
        int getCounter () const;

	// Increments the reference counter.
        void increment();

	// Decrements the reference counter.
        void decrement();

	// This function is used to determine whether to close an
	// HDF5 object when there are no more reference to that object.
	bool noReference();

	// Creates a reference counter to be used by an HDF5 object.
	RefCounter();

	~RefCounter();
#endif // DOXYGEN_SHOULD_SKIP_THIS

   private:
	int counter; // keeps track of number of copies of an object
};
#ifndef H5_NO_NAMESPACE
}
#endif
#endif
