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

#ifndef _IdComponent_H
#define _IdComponent_H

// IdComponent provides a mechanism to handle
// reference counting for an identifier of any HDF5 object.

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

class H5_DLLCPP IdComponent {
   public:
	// Parent classes must reset the current IdComponent copy
	// before setting new id to control reference count
	void setId( hid_t new_id );

	// Creates an object to hold an HDF5 identifier
	IdComponent( const hid_t h5_id );

	// Copy constructor: makes copy of the original IdComponent object.
	IdComponent( const IdComponent& original );

	// Gets the value of IdComponent's data member
	virtual hid_t getId () const;

	// Increment reference counter
	void incRefCount();

	// Decrement reference counter
	void decRefCount();

	// Get the reference counter to this identifier
	int getCounter();

	// Assignment operator
	IdComponent& operator=( const IdComponent& rhs );

	void reset();
	void resetId();

	// Destructor
	virtual ~IdComponent();

   protected:
	hid_t id;	// HDF5 object id

	// Default constructor
	IdComponent();

	// Gets the id of the H5 file in which the given object is located.
	hid_t p_get_file_id();

	// Creates a reference to an HDF5 object or a dataset region.
	void* p_reference(const char* name, hid_t space_id, H5R_type_t ref_type) const;
	
	// Retrieves the type of object that an object reference points to.
	H5G_obj_t p_get_obj_type(void *ref, H5R_type_t ref_type) const;

	// Retrieves a dataspace with the region pointed to selected.
	hid_t p_get_region(void *ref, H5R_type_t ref_type) const;

}; // end class IdComponent

#ifndef H5_NO_NAMESPACE
}
#endif
#endif
