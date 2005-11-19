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

#include <string>

#ifndef H5_NO_NAMESPACE
#ifndef H5_NO_STD
    using std::string;
#endif  // H5_NO_STD
#endif

#include "H5Include.h"
#include "H5Exception.h"
#include "H5IdComponent.h"
#include "H5PropList.h"
#include "H5Object.h"
#include "H5AbstractDs.h"
#include "H5DcreatProp.h"
#include "H5CommonFG.h"
#include "H5Alltypes.h"

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

//--------------------------------------------------------------------------
// Function:	AbstractDs default constructor
///\brief	Default constructor
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
AbstractDs::AbstractDs() : H5Object() {}

//--------------------------------------------------------------------------
// Function:	AbstractDs default constructor
///\brief	Creates an AbstractDs instance using an existing id.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
AbstractDs::AbstractDs( const hid_t ds_id ) : H5Object( ds_id ) {}

//--------------------------------------------------------------------------
// Function:	AbstractDs copy constructor
///\brief	Copy constructor: makes a copy of the original AbstractDs object.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
AbstractDs::AbstractDs( const AbstractDs& original ) : H5Object( original ) {}

//--------------------------------------------------------------------------
// Function:	AbstractDs::getTypeClass
///\brief	Returns the class of the datatype that is used by this
///		object, which can be a dataset or an attribute.
///\return	Datatype class identifier
///\exception	H5::DataTypeIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
H5T_class_t AbstractDs::getTypeClass() const
{
   // Gets the datatype used by this dataset or attribute.
   // p_get_type calls either H5Dget_type or H5Aget_type depending on
   // which object invokes getTypeClass
   DataType datatype(p_get_type());

   // Gets the class of the datatype and validate it before returning
   H5T_class_t type_class = H5Tget_class( datatype.getId());
   if( type_class != H5T_NO_CLASS )
      return( type_class );
   else
   {
      throw DataTypeIException(inMemFunc("getTypeClass"),
		"H5Tget_class returns H5T_NO_CLASS");
   }
}

//--------------------------------------------------------------------------
// Function:	AbstractDs::getDataType
///\brief	Returns the generic datatype of this abstract dataset, which
///		can be a dataset or an attribute.
///\return	DataType instance
///\exception	H5::DataTypeIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
DataType AbstractDs::getDataType() const
{
   // Gets the id of the datatype used by this dataset or attribute.
   // p_get_type calls either H5Dget_type or H5Aget_type depending on
   // which object invokes getTypeClass
   hid_t datatype_id = p_get_type();  // returned value is already validated

   // Create and return the DataType object
   DataType datatype( datatype_id );
   return( datatype );
}

//--------------------------------------------------------------------------
// Function:	AbstractDs::getArrayType
///\brief	Returns the compound datatype of this abstract dataset which
///		can be a dataset or an attribute.
///\return	ArrayType instance
///\exception	H5::DataTypeIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
ArrayType AbstractDs::getArrayType() const
{
   ArrayType arraytype(p_get_type());
   return(arraytype);
}

//--------------------------------------------------------------------------
// Function:	AbstractDs::getCompType
///\brief	Returns the compound datatype of this abstract dataset which
///		can be a dataset or an attribute.
///\return	CompType instance
///\exception	H5::DataTypeIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
CompType AbstractDs::getCompType() const
{
   CompType comptype(p_get_type());
   return(comptype);
}

//--------------------------------------------------------------------------
// Function:	AbstractDs::getEnumType
///\brief	Returns the enumeration datatype of this abstract dataset which
///		can be a dataset or an attribute.
///\return	EnumType instance
///\exception	H5::DataTypeIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
EnumType AbstractDs::getEnumType() const
{
   EnumType enumtype(p_get_type());
   return(enumtype);
}

//--------------------------------------------------------------------------
// Function:	AbstractDs::getFloatType
///\brief	Returns the floating-point datatype of this abstract dataset,
///		which can be a dataset or an attribute.
///\return	FloatType instance
///\exception	H5::DataTypeIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
FloatType AbstractDs::getFloatType() const
{
   FloatType floatype(p_get_type());
   return(floatype);
}

//--------------------------------------------------------------------------
// Function:	AbstractDs::getIntType
///\brief	Returns the integer datatype of this abstract dataset which
///		can be a dataset or an attribute.
///\return	IntType instance
///\exception	H5::DataTypeIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
IntType AbstractDs::getIntType() const
{
   IntType inttype(p_get_type());
   return(inttype);
}

//--------------------------------------------------------------------------
// Function:	AbstractDs::getStrType
///\brief	Returns the string datatype of this abstract dataset which
///		can be a dataset or an attribute.
///\return	StrType instance
///\exception	H5::DataTypeIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
StrType AbstractDs::getStrType() const
{
   StrType strtype(p_get_type());
   return(strtype);
}

//--------------------------------------------------------------------------
// Function:	AbstractDs::getVarLenType
///\brief	Returns the floating-point datatype of this abstract dataset,
///		which can be a dataset or an attribute.
///\return	VarLenType instance
///\exception	H5::DataTypeIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
VarLenType AbstractDs::getVarLenType() const
{
   VarLenType varlentype(p_get_type());
   return(varlentype);
}

//--------------------------------------------------------------------------
// Function:	AbstractDs destructor
///\brief	Noop destructor.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
AbstractDs::~AbstractDs() {}

#ifndef H5_NO_NAMESPACE
} // end namespace
#endif
