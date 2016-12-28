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
#include "H5ArrayType.h"

namespace H5 {

//--------------------------------------------------------------------------
// Function:	ArrayType default constructor
///\brief	Default constructor: Creates a stub ArrayType
// Programmer	Binh-Minh Ribler - May 2004
//--------------------------------------------------------------------------
ArrayType::ArrayType() : DataType() {}

//--------------------------------------------------------------------------
// Function:	ArrayType overloaded constructor
///\brief	Creates an ArrayType object using an existing id.
///\param	existing_id - IN: Id of an existing datatype
///\exception	H5::DataTypeIException
// Programmer	Binh-Minh Ribler - May 2004
//--------------------------------------------------------------------------
ArrayType::ArrayType( const hid_t existing_id ) : DataType( existing_id ) {}

//--------------------------------------------------------------------------
// Function:	ArrayType copy constructor
///\brief	Copy constructor: makes a copy of the original ArrayType object.
// Programmer	Binh-Minh Ribler - May 2004
//--------------------------------------------------------------------------
ArrayType::ArrayType(const ArrayType& original) : DataType(original) {}

//--------------------------------------------------------------------------
// Function:	ArrayType overloaded constructor
///\brief	Creates a new array data type based on the specified
///		\a base_type.
///\param	base_type - IN: Existing datatype
///\param	ndims     - IN: Rank of the array, [0..H5S_MAX_RANK]
///\param	dims      - IN: Size of each array dimension
///\exception	H5::DataTypeIException
// Programmer	Binh-Minh Ribler - May 2004
//--------------------------------------------------------------------------
ArrayType::ArrayType(const DataType& base_type, int ndims, const hsize_t* dims) : DataType()
{
    // Call C API to create an array data type
    hid_t new_type_id = H5Tarray_create2(base_type.getId(), ndims, dims);
    if (new_type_id < 0)
	throw DataTypeIException("ArrayType constructor", "H5Tarray_create2 failed");

    // Set the id for this object
    id = new_type_id;
}

//--------------------------------------------------------------------------
// Function:	ArrayType overloaded constructor
///\brief	Creates an ArrayType instance by opening an HDF5 array datatype
///		given its name, provided as a C character string.
///\param	dtype_name - IN: Array type name
///\exception	H5::DataTypeIException
// Programmer	Binh-Minh Ribler - Dec 2016
// Description
//		In 1.10.1, this constructor was introduced and will replace the
//		existing function CommonFG::openArrayType(const char*) to
//		improve usability.
//		-BMR, Dec 2016
//--------------------------------------------------------------------------
ArrayType::ArrayType(const H5Location& loc, const char *dtype_name) : DataType()
{
   id = p_opentype(loc, dtype_name);
}

//--------------------------------------------------------------------------
// Function:	ArrayType overloaded constructor
///\brief	Creates an ArrayType instance by opening an HDF5 array datatype
///		given its name, provided as an \c H5std_string.
///\param	dtype_name - IN: Array type name
///\exception	H5::DataTypeIException
// Programmer	Binh-Minh Ribler - Dec 2016
// Description
//		In 1.10.1, this constructor was introduced and will replace the
//		existing function CommonFG::openArrayType(const H5std_string&)
//		to improve usability.
//		-BMR, Dec 2016
//--------------------------------------------------------------------------
ArrayType::ArrayType(const H5Location& loc, const H5std_string& dtype_name) : DataType()
{
   id = p_opentype(loc, dtype_name.c_str());
}

//--------------------------------------------------------------------------
// Function:	ArrayType::operator=
///\brief	Assignment operator
///\param	rhs - IN: Reference to the existing array datatype
///\return	Reference to ArrayType instance
///\exception	H5::DataTypeIException
// Description
// 		Closes the id on the lhs object first with setId, then copies
//		each data member from the rhs object. (Issue HDFFV-9562)
// Programmer	Binh-Minh Ribler - Mar 2016
// Modification
//--------------------------------------------------------------------------
ArrayType& ArrayType::operator=(const ArrayType& rhs)
{
    if (this != &rhs)
    {
        // handling references to this id
        try {
            setId(rhs.id);
            // Note: a = b, so there are two objects with the same hdf5 id
            // that's why incRefCount is needed, and it is called by setId
        }
        catch (Exception& close_error) {
            throw DataTypeIException(inMemFunc("operator="), close_error.getDetailMsg());
        }
    }
    return(*this);
}

//--------------------------------------------------------------------------
// Function:	ArrayType::getArrayNDims
///\brief	Returns the number of dimensions for an array datatype.
///\return	Number of dimensions
///\exception	H5::DataTypeIException
// Programmer	Binh-Minh Ribler - May 2004
// Modification
//	Apr, 2016
//		Became const.
//--------------------------------------------------------------------------
int ArrayType::getArrayNDims() const
{
    // Get the rank of the array type specified by id from the C API
    int ndims = H5Tget_array_ndims(id);
    if (ndims < 0)
    {
	throw DataTypeIException("ArrayType::getArrayNDims", "H5Tget_array_ndims failed");
    }

    return(ndims);
}

//--------------------------------------------------------------------------
// Function:	ArrayType::getArrayDims
///\brief	Retrieves the size of all dimensions of an array datatype.
///\param	dims - OUT: Sizes of dimensions
///\return	Number of dimensions
///\exception	H5::DataTypeIException
// Programmer	Binh-Minh Ribler - May 2004
// Modification
//	Apr, 2016
//		Became const.
//--------------------------------------------------------------------------
int ArrayType::getArrayDims(hsize_t* dims) const
{
    // Get the dimensions
    int ndims = H5Tget_array_dims2(id, dims);
    if (ndims < 0)
	throw DataTypeIException("ArrayType::getArrayDims", "H5Tget_array_dims2 failed");

    // Return the number of dimensions
    return(ndims);
}

//--------------------------------------------------------------------------
// Function:	ArrayType destructor
///\brief	Properly terminates access to this array datatype.
// Programmer	Binh-Minh Ribler - May 2004
//--------------------------------------------------------------------------
ArrayType::~ArrayType() {}

} // end namespace
