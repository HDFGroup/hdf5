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

#include <string>

#include "H5Include.h"
#include "H5Exception.h"
#include "H5IdComponent.h"
#include "H5PropList.h"
#include "H5OcreatProp.h"
#include "H5DcreatProp.h"
#include "H5LaccProp.h"
#include "H5Location.h"
#include "H5Object.h"
#include "H5DataType.h"
#include "H5VarLenType.h"

namespace H5 {

//--------------------------------------------------------------------------
// Function:	VarLenType default constructor
///\brief	Default constructor: Creates a stub variable-length datatype.
//--------------------------------------------------------------------------
VarLenType::VarLenType() : DataType() {}

//--------------------------------------------------------------------------
// Function:	VarLenType overloaded constructor
///\brief	Creates an VarLenType object using an existing id.
///\param	existing_id - IN: Id of an existing datatype
///\exception	H5::DataTypeIException
// Programmer	Binh-Minh Ribler - May, 2004
//--------------------------------------------------------------------------
VarLenType::VarLenType(const hid_t existing_id) : DataType(existing_id) {}

//--------------------------------------------------------------------------
// Function:	VarLenType copy constructor
///\brief	Copy constructor: makes a copy of the original VarLenType object.
// Programmer	Binh-Minh Ribler - May, 2004
//--------------------------------------------------------------------------
VarLenType::VarLenType(const VarLenType& original) : DataType(original) {}

//--------------------------------------------------------------------------
// Function:	VarLenType overloaded constructor
///\brief	Creates a new variable-length datatype based on the specified
///		\a base_type.
///\param	base_type - IN: Pointer to existing datatype
///\exception	H5::DataTypeIException
// Description
//		DataType passed by pointer to avoid clashing with copy
//		constructor.
// Programmer	Binh-Minh Ribler - May, 2004
//--------------------------------------------------------------------------
VarLenType::VarLenType(const DataType* base_type) : DataType()
{
   id = H5Tvlen_create(base_type->getId());
   if (id < 0)
   {
      throw DataTypeIException("VarLenType constructor",
                "H5Tvlen_create returns negative value");
   }
}

//--------------------------------------------------------------------------
// Function:	VarLenType overloaded constructor
///\brief	Creates an VarLenType instance by opening an HDF5 variable
///		length datatype given its name, provided as a C char*.
///\param	dtype_name - IN: Variable length type name
///\exception	H5::DataTypeIException
// Programmer	Binh-Minh Ribler - Dec 2016
// Description
//		In 1.10.1, this constructor was introduced and will replace the
//		existing function CommonFG::openVarLenType(const char*) to
//		improve usability.
//		-BMR, Dec 2016
//--------------------------------------------------------------------------
VarLenType::VarLenType(const H5Location& loc, const char *dtype_name) : DataType()
{
   id = p_opentype(loc, dtype_name);
}

//--------------------------------------------------------------------------
// Function:	VarLenType overloaded constructor
///\brief	Creates an VarLenType instance by opening an HDF5 variable
///		length datatype given its name, provided as an \c H5std_string.
///\param	dtype_name - IN: Variable length type name
///\exception	H5::DataTypeIException
// Programmer	Binh-Minh Ribler - Dec 2016
// Description
//		In 1.10.1, this constructor was introduced and will replace the
//		existing function CommonFG::openVarLenType(const H5std_string&)
//		to improve usability.
//		-BMR, Dec 2016
//--------------------------------------------------------------------------
VarLenType::VarLenType(const H5Location& loc, const H5std_string& dtype_name) : DataType()
{
   id = p_opentype(loc, dtype_name.c_str());
}

//--------------------------------------------------------------------------
// Function:	VarLenType destructor
///\brief	Properly terminates access to this datatype.
// Programmer	Binh-Minh Ribler - May, 2004
//--------------------------------------------------------------------------
VarLenType::~VarLenType() {}

} // end namespace
