#include <string>

#include "H5Include.h"
#include "H5RefCounter.h"
#include "H5Exception.h"
#include "H5IdComponent.h"
#include "H5PropList.h"
#include "H5Object.h"
#include "H5AbstractDs.h"
#include "H5Alltypes.h"

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

// Default constructor
AbstractDs::AbstractDs() : H5Object() {}

// Constructor that takes an id
AbstractDs::AbstractDs( const hid_t ds_id ) : H5Object( ds_id ) {}

// Copy constructor: makes copy of the original object; simply invokes
// base-class copy constructor.
AbstractDs::AbstractDs( const AbstractDs& original ) : H5Object( original ) {}

// Returns the class of the datatype that is used by this dataset
H5T_class_t AbstractDs::getTypeClass() const
{
   // Gets the datatype used by this dataset or attribute.
   // p_getType calls either H5Dget_type or H5Aget_type depending on 
   // which object invokes getTypeClass
   DataType datatype( p_getType());

   // Gets the class of the datatype and validate it before returning
   H5T_class_t type_class = H5Tget_class( datatype.getId());
   if( type_class != H5T_NO_CLASS )
      return( type_class );
   else
   {
      throw DataTypeIException();
   }
}

// Returns the generic datatype of this abstract dataset which 
// can be a dataset or an attribute.
DataType AbstractDs::getDataType() const
{
   // Gets the id of the datatype used by this dataset or attribute.
   // p_getType calls either H5Dget_type or H5Aget_type depending on 
   // which object invokes getTypeClass
   hid_t datatype_id = p_getType();  // returned value is already validated

   // Create and return the DataType object
   DataType datatype( datatype_id );
   return( datatype );
}

// Returns the enumeration datatype of this abstract dataset which 
// can be a dataset or an attribute.
EnumType AbstractDs::getEnumType() const
{
   EnumType enumtype( p_getType());
   return( enumtype );
}

// Returns the compound datatype of this abstract dataset which 
// can be a dataset or an attribute.
CompType AbstractDs::getCompType() const
{
   CompType comptype( p_getType());
   return( comptype );
}

// Returns the integer datatype of this abstract dataset which 
// can be a dataset or an attribute.
IntType AbstractDs::getIntType() const
{
   IntType inttype( p_getType());
   return( inttype );
}

// Returns the floating-point datatype of this abstract dataset which 
// can be a dataset or an attribute.
FloatType AbstractDs::getFloatType() const
{
   FloatType floatype( p_getType());
   return( floatype );
}

// Returns the string datatype of this abstract dataset which 
// can be a dataset or an attribute.
StrType AbstractDs::getStrType() const
{
   StrType strtype( p_getType());
   return( strtype );
}

/* This version of getDataType is older style.  New style above doesn't
use overloading.  Remove it when knowing for sure that the other way
is prefered
// Gets the specific datatype of this abstract dataset which can be a 
// dataset or an attribute.  Several overloaded getDataType's below
// are for specific sub-datatypes.
void AbstractDs::getDataType( EnumType& enumtype ) const
{
   enumtype.setId( p_getType());
}

void AbstractDs::getDataType( CompType& comptype ) const
{
   comptype.setId( p_getType());
}

void AbstractDs::getDataType( IntType& inttype ) const
{
   inttype.setId( p_getType());
}

void AbstractDs::getDataType( FloatType& floatype ) const
{
   floatype.setId( p_getType());
}

void AbstractDs::getDataType( StrType& strtype ) const
{
   strtype.setId( p_getType());
}
end of old style of getDataType */

// Default destructor
AbstractDs::~AbstractDs() {}

#ifndef H5_NO_NAMESPACE
} // end namespace
#endif
