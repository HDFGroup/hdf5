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

#ifndef _H5Object_H
#define _H5Object_H

#include "H5Classes.h"		// constains forward class declarations

// H5Object is a baseclass.  It has these subclasses:
// Group, AbstractDs, and DataType.
// AbstractDs, in turn, has subclasses DataSet and Attribute.
// DataType, in turn, has several specific datatypes as subclasses.

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

#ifndef DOXYGEN_SHOULD_SKIP_THIS
class H5_DLLCPP H5Object;  // forward declaration for UserData4Aiterate

// Define the operator function pointer for H5Aiterate().
typedef void (*attr_operator_t)( H5Object& loc/*in*/,
				 const string attr_name/*in*/,
				 void *operator_data/*in,out*/);

class UserData4Aiterate { // user data for attribute iteration
   public:
	unsigned int* idx;
	attr_operator_t op;
	void* opData;
	H5Object* object;
};
#endif // DOXYGEN_SHOULD_SKIP_THIS

// The above part is being moved into Iterator, but not completed

class H5_DLLCPP H5Object : public IdComponent {
   public:
	// Creates an attribute for a group, dataset, or named datatype.
	// PropList is currently not used, so always be default.
	Attribute createAttribute( const char* name, const DataType& type, const DataSpace& space, const PropList& create_plist = PropList::DEFAULT ) const;
	Attribute createAttribute( const string& name, const DataType& type, const DataSpace& space, const PropList& create_plist = PropList::DEFAULT ) const;

	// Opens an attribute given its name.
	Attribute openAttribute( const char* name ) const;
	Attribute openAttribute( const string& name ) const;

	// Opens an attribute given its index.
	Attribute openAttribute( const unsigned int idx ) const;

	// Flushes all buffers associated with this object to disk
	void flush( H5F_scope_t scope ) const;

	// Gets the name of the file, in which this HDF5 object belongs.
	string getFileName() const;

	// Determines the number of attributes attached to this object.
	int getNumAttrs() const;

	// Iterate user's function over the attributes of this object
	int iterateAttrs( attr_operator_t user_op, unsigned* idx = NULL, void* op_data = NULL );

	// Removes the named attribute from this object.
	void removeAttr( const char* name ) const;
	void removeAttr( const string& name ) const;

	// Renames the attribute to a new name.
	void renameAttr(const char* oldname, const char* newname) const;
	void renameAttr(const string& oldname, const string& newname) const;

	// Copy constructor: makes copy of an H5Object object.
	H5Object(const H5Object& original);

	// Noop destructor.
	virtual ~H5Object();

   protected:
#ifndef DOXYGEN_SHOULD_SKIP_THIS
	// Default constructor
	H5Object();

	// Creates a copy of an existing object giving the object id
	H5Object( const hid_t object_id );
#endif // DOXYGEN_SHOULD_SKIP_THIS

}; /* end class H5Object */

#ifndef H5_NO_NAMESPACE
}
#endif
#endif
