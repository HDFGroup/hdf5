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

#ifndef __H5VarLenType_H
#define __H5VarLenType_H

namespace H5 {

/*! \class VarLenType
    \brief VarLenType is a derivative of a DataType and operates on HDF5
    C's Variable-length Datatypes.

    Inheritance: DataType -> H5Object -> H5Location -> IdComponent
*/
class H5_DLLCPP VarLenType : public DataType {
   public:
	// Constructor that creates a variable-length datatype based
	// on the specified base type.
	VarLenType(const DataType* base_type);

	///\brief Returns this class name.
	virtual H5std_string fromClass () const { return("VarLenType"); }

	// Copy constructor: makes copy of the original object.
	VarLenType( const VarLenType& original );

	// Constructor that takes an existing id
	VarLenType( const hid_t existing_id );

	// Constructors that open a variable-length datatype, given a location.
	VarLenType(const H5Location& loc, const char* name);
	VarLenType(const H5Location& loc, const H5std_string& name);

	// Noop destructor
	virtual ~VarLenType();

	// Default constructor
	VarLenType();
};
}
#endif // __H5VarLenType_H
