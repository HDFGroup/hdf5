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

#ifndef _H5Idtemplates_h
#define _H5Idtemplates_h

#include "H5IdComponent.h"

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

#ifndef DOXYGEN_SHOULD_SKIP_THIS
//--------------------------------------------------------------------------
// Function:	resetIdComponent
///\brief	Resets the id of the passed-in object.
///\param	obj - IN: A "this" pointer of an IdComponent object
///\exception	H5::Exception's subclasses, thrown by p_close
///\par Description:
///		This function is used to reset an IdComponent object 
///		before using it again for another HDF5 object.  If 
///		the member \a id is the valid id of an HDF5 object, which
///		this IdComponent object represents, the associate close
///		function will be called to properly close the HDF5 object.
//
//  Note:	Some older compilers don't support template member functions; 
//		so at this time, resetIdComponent is not a member function so 
//		it can be template to work around that problem.
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
template <class Type>
H5_DLLCPP void resetIdComponent( 
	Type* obj )	// pointer to object to be reset
{
   if( obj->noReference())  // ref count of this object is decremented here
   {
      if( obj->getId() > 0 )
      {
         obj->p_close();  // which p_close depends on whom this
                             // IdComponent object belongs to
      }
      obj->reset();  // delete ref_count from IdComponent
   }
}
#endif // DOXYGEN_SHOULD_SKIP_THIS
#ifndef H5_NO_NAMESPACE
}
#endif
#endif
