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

#ifndef H5Attribute_H_
#define H5Attribute_H_

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

class __DLLCPP__ Attribute : public AbstractDs {
   public:
	// Writes data to this attribute.
	void write(const DataType& mem_type, const void *buf ) const;

	// Reads data from this attribute.
	void read( const DataType& mem_type, void *buf ) const;

	// Gets a copy of the dataspace for this attribute.
	virtual DataSpace getSpace() const;

	// Gets the name of this attribute.
	ssize_t getName( size_t buf_size, string& attr_name ) const;
	string getName( size_t buf_size ) const; // returns name, not its length

	// do not inherit iterateAttrs from H5Object
	int iterateAttrs() { return 0; }

	// Used by the API to appropriately close a attribute
	virtual void p_close() const;

        // Creates a copy of an existing attribute using the attribute id
        Attribute( const hid_t attr_id );

	// Copy constructor: makes a copy of an existing Attribute object.
	Attribute( const Attribute& original );

	virtual ~Attribute();

   private:
	// This function contains the common code that is used by 
	// getTypeClass and various API functions getXxxType 
	// defined in AbstractDs for generic datatype and specific 
	// sub-types
	virtual hid_t p_getType() const;
};
#ifndef H5_NO_NAMESPACE
}
#endif
#endif
