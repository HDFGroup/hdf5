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

#include "H5Include.h"
#include "H5RefCounter.h"
#include "H5Exception.h"
#include "H5IdComponent.h"
#include "H5PropList.h"
#include "H5Object.h"
#include "H5Alltypes.h"
#include "H5AbstractDs.h"
#include "H5DxferProp.h"
#include "H5DataSpace.h"
#include "H5DataSet.h"
#include "H5private.h"

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

// Creates a new compound datatype
CompType::CompType( size_t size ) : DataType( H5T_COMPOUND, size ) {}

// Creates a compound datatype using an existing id
CompType::CompType( const hid_t existing_id ) : DataType( existing_id ) {}

// Default constructor
CompType::CompType() : DataType() {}

// Copy constructor: makes copy of the original CompType object
CompType::CompType( const CompType& original ) : DataType( original ) {}

// Gets the compound datatype of the specified dataset - reimplement this
CompType::CompType( const DataSet& dataset ) : DataType()
{
   // Calls C function H5Dget_type to get the id of the datatype
   id = H5Dget_type( dataset.getId() );

   // If the datatype id is invalid, throw exception
   if( id <= 0 )
   {
      throw DataSetIException("CompType constructor", "H5Dget_type failed");
   }
}

// Retrieves the number of members in this compound datatype. 
int CompType::getNmembers() const
{
   int num_members = H5Tget_nmembers( id );
   if( num_members < 0 )
   {
      throw DataTypeIException("CompType::getNmembers", 
		"H5Tget_nmembers returns negative number of members");
   }
   return( num_members );
}

// Retrieves the name of a member of this compound datatype. 
string CompType::getMemberName( int member_num ) const
{
    char* member_name_C = H5Tget_member_name( id, member_num );
    if( member_name_C == NULL )  // NULL means failure
    {
	throw DataTypeIException("CompType::getMemberName", 
		"H5Tget_member_name returns NULL for member name");
    }
    string member_name = string(member_name_C); // convert C string to string 
    HDfree(member_name_C); // free the C string 
    return( member_name ); // return the member name string
}

/*-------------------------------------------------------------------------
 * Function:    getMemberIndex
 *
 * Purpose:     Returns the index of a member in a compound data type.  
 *              Members are stored in no particular order with numbers 0 
 *              through N-1, where N is the value returned by the member 
 *              function getNmembers.
 *
 * Return:      Success:	index of the member if exists.
 *              Failure:	DataTypeIException
 *
 * BMR - June 10, 2002
 *-------------------------------------------------------------------------
 */
int CompType::getMemberIndex(const char* name) const
{
   int member_index = H5Tget_member_index(id, name);
   if( member_index < 0 )
   {
      throw DataTypeIException("CompType::getMemberIndex", 
		"H5Tget_member_index returns negative value");
   }
   return( member_index );
}
int CompType::getMemberIndex(const string& name) const
{
   return(getMemberIndex(name.c_str()));
}

/*-------------------------------------------------------------------------
 * Function:    getMemberOffset
 *
 * Purpose:     Returns the byte offset of the beginning of a member with
 *              respect to the beginning of the compound data type datum.
 *              Members are stored in no particular order with numbers 0 
 *              through N-1, where N is the value returned by the member 
 *              function getNmembers.
 *
 * Return:      Success:        Byte offset.
 *              Failure:        Quincey: for now, 0 is not a failure
 *
 * BMR - 2000
 *-------------------------------------------------------------------------
 */
size_t CompType::getMemberOffset( int member_num ) const
{
   size_t offset = H5Tget_member_offset( id, member_num );
   //if( offset == 0 )
   //{
      //throw DataTypeIException("CompType::getMemberOffset",
		//"H5Tget_member_offset failed");
   //}
   return( offset );
}

// Returns the dimensionality of the member. 
int CompType::getMemberDims( int member_num, size_t* dims, int* perm ) const
{
   throw DataTypeIException( "Error: getMemberDims is no longer supported." );
   return (-1); // unreachable statement; but without it, the compiler 
		// will complain
}

// Gets the type class of the specified member.
H5T_class_t CompType::getMemberClass( int member_num ) const
{
   // get the member datatype first
   hid_t member_type_id = H5Tget_member_type( id, member_num );
   if( member_type_id <= 0 )
   {
      throw DataTypeIException("CompType::getMemberClass", 
		"H5Tget_member_type failed");
   }

   // then get its class
   H5T_class_t member_class = H5Tget_class( member_type_id );
   if( member_class == H5T_NO_CLASS )
   {
      throw DataTypeIException("CompType::getMemberClass", 
		"H5Tget_class returns H5T_NO_CLASS");
   }
   return( member_class );
}

// This private member function calls the C API to get the identifier 
// of the specified member.  It provides the id to construct appropriate
// sub-types in the functions getMemberXxxType below, where Xxx indicates
// the sub-types.
hid_t CompType::p_getMemberType( int member_num ) const
{
   // get the id of the specified member first
   hid_t member_type_id = H5Tget_member_type( id, member_num );
   if( member_type_id > 0 )
      return( member_type_id );
   else
   {
	// p_getMemberType is private, use caller's function name for api
      throw DataTypeIException("CompType::getMemberDataType", 
		"H5Tget_member_type failed");
   }
}

// Returns the datatype of the specified member in this compound datatype. 
DataType CompType::getMemberDataType( int member_num ) const
{
   DataType datatype( p_getMemberType( member_num )); 
   return( datatype );
}

EnumType CompType::getMemberEnumType( int member_num ) const
{
   EnumType enumtype( p_getMemberType( member_num )); 
   return( enumtype );
}

CompType CompType::getMemberCompType( int member_num ) const
{
   CompType comptype( p_getMemberType( member_num )); 
   return( comptype );
}

IntType CompType::getMemberIntType( int member_num ) const
{
   IntType inttype( p_getMemberType( member_num )); 
   return( inttype );
}

FloatType CompType::getMemberFloatType( int member_num ) const
{
   FloatType floatype( p_getMemberType( member_num )); 
   return( floatype );
}

StrType CompType::getMemberStrType( int member_num ) const
{
   StrType strtype( p_getMemberType( member_num )); 
   return( strtype );
}

/* old style of getMemberType - using overloads; new style above 
   returns the appropriate datatypes but has different named functions.
   In the old style, a datatype must be passed into the function.
// Returns the datatype of the specified member in this compound datatype. 
// Several overloading of getMemberType are for different datatypes
void CompType::getMemberType( int member_num, EnumType& enumtype ) const
{
   p_getMemberType( member_num, enumtype ); 
}

void CompType::getMemberType( int member_num, CompType& comptype ) const
{
   p_getMemberType( member_num, comptype ); 
}

void CompType::getMemberType( int member_num, IntType& inttype ) const
{
   p_getMemberType( member_num, inttype ); 
}

void CompType::getMemberType( int member_num, FloatType& floatype ) const
{
   p_getMemberType( member_num, floatype ); 
}

void CompType::getMemberType( int member_num, StrType& strtype ) const
{
   p_getMemberType( member_num, strtype ); 
}
// end of overloading of getMemberType
*/

// Adds a new member to a compound datatype
void CompType::insertMember( const string& name, size_t offset, const DataType& new_member ) const
{
   // Convert string to C-string
   const char* name_C;
   name_C = name.c_str();  // name_C refers to the contents of name as a C-str

   hid_t new_member_id = new_member.getId();  // get new_member id for C API

   // Call C routine H5Tinsert to add the new member
   herr_t ret_value = H5Tinsert( id, name_C, offset, new_member_id );
   if( ret_value < 0 )
   {
      throw DataTypeIException("CompType::insertMember", "H5Tinsert failed");
   }
}

// Recursively removes padding from within a compound datatype. 
void CompType::pack() const
{
   // Calls C routine H5Tpack to remove padding
   herr_t ret_value = H5Tpack( id );
   if( ret_value < 0 )
   {
      throw DataTypeIException("CompType::pack", "H5Tpack failed");
   }
}

// This destructor just invokes the base-class' destructor
CompType::~CompType() {}

#ifndef H5_NO_NAMESPACE
} // end namespace
#endif
