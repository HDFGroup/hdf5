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
#include "H5Exception.h"
#include "H5RefCounter.h"

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif
#ifndef DOXYGEN_SHOULD_SKIP_THIS
//--------------------------------------------------------------------------
// Function:	RefCounter default constructor
// Purpose	Default constructor: Creates a reference counter and set it
//		to 1.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
RefCounter::RefCounter() : counter(1) {} 
 
//--------------------------------------------------------------------------
// Function:	RefCounter::getCounter
// Purpose	Returns the current value of the reference counter.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
int RefCounter::getCounter () const { return counter; }

//--------------------------------------------------------------------------
// Function:	RefCounter::increment
// Purpose	Increments the reference counter.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void RefCounter::increment() { counter++; }

//--------------------------------------------------------------------------
// Function:	RefCounter::decrement
// Purpose	Decrements the reference counter.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void RefCounter::decrement() 
{ 
   if (counter > 0)
      counter--;
   else
      throw IdComponentException("RefCounter::decrement",
                "reference counter of this object has non-positive value");
}

//--------------------------------------------------------------------------
// Function:	RefCounter::noReference
// Purpose	Returns true if there are no more reference to the object 
//		that uses this counter.
// Return	true if there are no more reference to the object
//		that uses this counter, and false, otherwise.
// Description
//		Decrements the reference counter then determines if there 
//		are no more reference to the object that uses this counter.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
bool RefCounter::noReference()
{
   if (counter > 0)
      counter--;
   return(counter <= 0 ? true:false);
}

//--------------------------------------------------------------------------
// Function:	RefCounter destructor
// Purpose	Noop destructor.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
RefCounter::~RefCounter() {}
#endif // DOXYGEN_SHOULD_SKIP_THIS

#ifndef H5_NO_NAMESPACE
} // end namespace
#endif
