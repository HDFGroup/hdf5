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

#ifndef _H5Location_H
#define _H5Location_H

#include "H5Classes.h"		// constains forward class declarations

// H5Location is an abstract class.  It provides a collection of wrappers
// of C functions which take location IDs.  Most of these were in H5Object
// but are now moved here for H5File's access.

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

class H5_DLLCPP H5Location;  // forward declaration for UserData4Aiterate

// Define the operator function pointer for H5Aiterate().
typedef void (*attr_operator_t)( H5Location& loc/*in*/,
                                 const H5std_string attr_name/*in*/,
                                 void *operator_data/*in,out*/);

class UserData4Aiterate { // user data for attribute iteration
   public:
        attr_operator_t op;
        void* opData;
        H5Location* location;
};

// An H5Location can be a file, group, dataset, named datatype, or attribute.

class H5_DLLCPP H5Location : public IdComponent {
   public:
	// Creates an attribute for the specified object at this location
	// PropList is currently not used, so always be default.
	Attribute createAttribute( const char* name, const DataType& type, const DataSpace& space, const PropList& create_plist = PropList::DEFAULT ) const;
	Attribute createAttribute( const H5std_string& name, const DataType& type, const DataSpace& space, const PropList& create_plist = PropList::DEFAULT ) const;

	// Given its name, opens the attribute that belongs to an object at
	// this location.
	Attribute openAttribute( const char* name ) const;
	Attribute openAttribute( const H5std_string& name ) const;

	// Given its index, opens the attribute that belongs to an object at
	// this location.
	Attribute openAttribute( const unsigned int idx ) const;

	// Flushes all buffers associated with this location to disk.
	void flush( H5F_scope_t scope ) const;

	// Gets the name of the file, specified by this location.
	H5std_string getFileName() const;

	// Determines the number of attributes at this location.
	int getNumAttrs() const;

#ifndef H5_NO_DEPRECATED_SYMBOLS
	// Retrieves the type of object that an object reference points to.
	H5G_obj_t getObjType(void *ref, H5R_type_t ref_type = H5R_OBJECT) const;
#endif /* H5_NO_DEPRECATED_SYMBOLS */

	// Retrieves the type of object that an object reference points to.
	H5O_type_t getRefObjType(void *ref, H5R_type_t ref_type = H5R_OBJECT) const;
	// Note: getRefObjType deprecates getObjType, but getObjType's name is
	// misleading, so getRefObjType is used in the new function instead.

	// Iterate user's function over the attributes at this location.
	int iterateAttrs( attr_operator_t user_op, unsigned* idx = NULL, void* op_data = NULL );

	// Removes the named attribute from this location.
	void removeAttr( const char* name ) const;
	void removeAttr( const H5std_string& name ) const;

	// Renames the named attribute to a new name.
	void renameAttr(const char* oldname, const char* newname) const;
	void renameAttr(const H5std_string& oldname, const H5std_string& newname) const;

	// Creates a reference to a named object or to a dataset region
	// in this object.
	void reference(void* ref, const char* name, const DataSpace& dataspace,
			H5R_type_t ref_type = H5R_DATASET_REGION) const;
	void reference(void* ref, const char* name) const;
	void reference(void* ref, const H5std_string& name) const;

	// Open a referenced object whose location is specified by either
	// a file, an HDF5 object, or an attribute.
	void dereference(H5File& h5file, const void* ref, H5R_type_t ref_type = H5R_OBJECT);
	void dereference(H5Object& obj, const void* ref, H5R_type_t ref_type = H5R_OBJECT);
	void dereference(Attribute& attr, const void* ref, H5R_type_t ref_type = H5R_OBJECT);

        // For subclasses.
        virtual hid_t getId() const = 0;

   protected:
#ifndef DOXYGEN_SHOULD_SKIP_THIS
	// Default constructor,
	H5Location();

	// Creates a copy of an existing object giving the location id.
	H5Location(const hid_t loc_id);

	// Copy constructor.
	H5Location(const H5Location& original);

	// Creates a reference to an HDF5 object or a dataset region.
	void p_reference(void* ref, const char* name, hid_t space_id, H5R_type_t ref_type) const;

	// Dereferences a ref into an HDF5 id.
	hid_t p_dereference(hid_t loc_id, const void* ref, H5R_type_t ref_type);

#ifndef H5_NO_DEPRECATED_SYMBOLS
	// Retrieves the type of object that an object reference points to.
	H5G_obj_t p_get_obj_type(void *ref, H5R_type_t ref_type) const;
#endif /* H5_NO_DEPRECATED_SYMBOLS */

	// Retrieves the type of object that an object reference points to.
	H5O_type_t p_get_ref_obj_type(void *ref, H5R_type_t ref_type) const;

	// Retrieves a dataspace with the region pointed to selected.
	hid_t p_get_region(void *ref, H5R_type_t ref_type) const;

	// Noop destructor.
	virtual ~H5Location();

#endif // DOXYGEN_SHOULD_SKIP_THIS

}; /* end class H5Location */

#ifndef H5_NO_NAMESPACE
}
#endif
#endif
