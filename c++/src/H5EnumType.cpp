#include <string>

#include "H5Include.h"
#include "H5RefCounter.h"
#include "H5Exception.h"
#include "H5IdComponent.h"
#include "H5PropList.h"
#include "H5Object.h"
#include "H5AbstractDs.h"
#include "H5DxferProp.h"
#include "H5DataSpace.h"
#include "H5DataType.h"
#include "H5DataSet.h"
#include "H5AtomType.h"
#include "H5IntType.h"
#include "H5EnumType.h"

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

// Creates an empty enumeration datatype based on a native signed
// integer type.
EnumType::EnumType( size_t size ) : DataType( H5T_ENUM, size ) {}

// Default constructor
EnumType::EnumType() : DataType() {}

// Creates a enumeration datatype using an existing id
EnumType::EnumType( const hid_t existing_id ) : DataType( existing_id ) {}

// Copy constructor: makes a copy of the original EnumType object. 
EnumType::EnumType( const EnumType& original ) : DataType( original ) {}

// Gets the enum datatype of the specified dataset 
EnumType::EnumType( const DataSet& dataset ) : DataType()
{
   // Calls C function H5Dget_type to get the id of the datatype
   id = H5Dget_type( dataset.getId() );

   // If the datatype id is not valid, throw an exception
   if( id <= 0 )
   {
      throw DataSetIException("Getting datatype fails...");
   }
}

// Creates a new enum datatype based on an integer datatype
EnumType::EnumType( const IntType& data_type ) : DataType()
{
   // Calls C function H5Tenum_create to get the id of the datatype
   id = H5Tenum_create( data_type.getId() );

   // If the datatype id is not valid, throw an exception
   if( id <= 0 )
   {
      throw DataSetIException("Creating enumeration datatype fails...");
   }
}

// Inserts a new member to this enumeration datatype.
void EnumType::insert( const string& name, void *value ) const
{
    insert( name.c_str(), value );
}
void EnumType::insert( const char* name, void *value ) const
{
   // Calls C routine H5Tenum_insert to insert the new enum datatype member. 
   herr_t ret_value = H5Tenum_insert( id, name, value );
   if( ret_value < 0 )
   {
      throw DataTypeIException();
   }
}

// Returns the symbol name corresponding to a specified member of an enumeration datatype. 
string EnumType::nameOf( void *value, size_t size ) const
{
   char* name_C = new char[size+1];  // temporary C-string for C API

   // Calls C routine H5Tenum_nameof to get the name of the specified enum type
   herr_t ret_value = H5Tenum_nameof( id, value, name_C, size );

   // If H5Tenum_nameof returns a negative value, raise an exception,
   if( ret_value < 0 )
   {
      throw DataTypeIException();
   }
   // otherwise, create the string to hold the datatype name and return it
   string name = string( name_C );
   delete name_C;
   return( name );
}

// Retrieves the value corresponding to a member of an enumeration 
// datatype, given the member's name. 
void EnumType::valueOf( const string& name, void *value ) const
{
    valueOf( name.c_str(), value );
}
void EnumType::valueOf( const char* name, void *value ) const
{
   // Calls C routine H5Tenum_valueof to get the enum datatype value
   herr_t ret_value = H5Tenum_valueof( id, name, value );
   if( ret_value < 0 )
   {
      throw DataTypeIException();
   }
}

// Retrieves the value of a member in this enumeration datatype, given the 
// member's index.
void EnumType::getMemberValue( int memb_no, void *value ) const
{
   // Call C routine H5Tget_member_value to get the datatype member's value
   hid_t ret_value = H5Tget_member_value( id, memb_no, value );
   if( ret_value < 0 )
   {
      throw DataTypeIException();
   }
}

// Default destructor
EnumType::~EnumType() {}

#ifndef H5_NO_NAMESPACE
} // end namespace
#endif
