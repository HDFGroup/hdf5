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

#ifndef _H5PropList_H
#define _H5PropList_H

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

class  __DLLCPP__ PropList : public IdComponent {
   public:
	// Default property list
        static const PropList DEFAULT;

	// Creates a property list given the property list type.
	PropList( H5P_class_t type );

	// Default constructor: creates a PropList object - this object
	// does not represent any property list yet.
	PropList();

	// Copy constructor: creates a copy of a PropList object.
	PropList( const PropList& original );

	// Makes a copy of the given property list.
	void copy( const PropList& like_plist );

	// Make a copy of the given property list using assignment statement
	PropList& operator=( const PropList& rhs );

	// Gets the class of this property list, i.e. H5P_FILE_CREATE,
	// H5P_FILE_ACCESS, ...
	H5P_class_t getClass() const;

	// Creates a default property list or creates a copy of an 
	// existing property list giving the property list id
	PropList( const hid_t plist_id );

	// Used by the API to close the property list
	void p_close() const;

	virtual ~PropList();
};

#ifndef H5_NO_NAMESPACE
}
#endif
#endif  // _H5PropList_H
