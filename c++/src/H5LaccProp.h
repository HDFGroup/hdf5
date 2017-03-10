// C++ informative line for the emacs editor: -*- C++ -*-
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

// Class LinkAccPropList represents the HDF5 file access property list and
// inherits from DataType.

#ifndef __H5LinkAccPropList_H
#define __H5LinkAccPropList_H

namespace H5 {

/*! \class LinkAccPropList
    \brief Class LinkAccPropList inherits from PropList and provides
    wrappers for the HDF5 file access property list.

    Inheritance: PropList -> IdComponent
*/
class H5_DLLCPP LinkAccPropList : public PropList {
   public:
	///\brief Default file access property list.
	static const LinkAccPropList& DEFAULT;

	// Creates a file access property list.
	LinkAccPropList();

	///\brief Returns this class name.
	virtual H5std_string fromClass () const { return("LinkAccPropList"); }

	// Copy constructor: creates a copy of a LinkAccPropList object.
	LinkAccPropList( const LinkAccPropList& original );

	// Creates a copy of an existing file access property list
	// using the property list id.
	LinkAccPropList (const hid_t plist_id);

	// Noop destructor
	virtual ~LinkAccPropList();

#ifndef DOXYGEN_SHOULD_SKIP_THIS

        // Deletes the global constant, should only be used by the library
        static void deleteConstants();

    private:
        static LinkAccPropList* DEFAULT_;

        // Creates the global constant, should only be used by the library
        static LinkAccPropList* getConstant();

#endif // DOXYGEN_SHOULD_SKIP_THIS

};
}
#endif // __H5LinkAccPropList_H
