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

#include "H5Include.h"
#include "H5RefCounter.h"

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

// Creates a reference counter to be used by an HDF5 object
RefCounter::RefCounter() : counter(1) {} 
 
// Returns the current value of the reference counter
int RefCounter::getCounter () const { return counter; }

// Increments the reference counter as a copy of the object that uses
// this counter is created.
void RefCounter::increment() { counter++; }

// Decrements the reference counter as a copy of the object that uses
// this counter is destroyed.
void RefCounter::decrement() { counter--; }

// Decrements the reference counter then determines if there are no more 
// reference to the object that uses this counter
bool RefCounter::noReference()
{
   if( counter > 0 )
      counter--;
   return( counter == 0 ? true:false );
}

RefCounter::~RefCounter() {}

#ifndef H5_NO_NAMESPACE
} // end namespace
#endif
