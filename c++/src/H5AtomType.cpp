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
#include "H5DataType.h"
#include "H5AtomType.h"

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

// Default constructor
AtomType::AtomType() : DataType() {}

// Constructor that takes an existing id 
AtomType::AtomType( const hid_t existing_id ) : DataType( existing_id ) {}

// Copy constructor: makes a copy of the original AtomType object.
AtomType::AtomType( const AtomType& original ) : DataType( original ) {}

// Sets the total size for an atomic datatype. 
void AtomType::setSize( size_t size ) const
{
   // Call C routine H5Tset_size to set the total size
   herr_t ret_value = H5Tset_size( id, size );
   if( ret_value < 0 )
   {
      throw DataTypeIException("AtomType::setSize", "H5Tset_size failed");
   }
}

// Returns the byte order of an atomic datatype.  Inheritance class???
H5T_order_t AtomType::getOrder( string& order_string ) const
{
   // Call C routine to get the byte ordering
   H5T_order_t type_order = H5Tget_order( id );

   // return a byte order constant if successful
   if( type_order == H5T_ORDER_ERROR )
   {
      throw DataTypeIException("AtomType::getOrder", 
		"H5Tget_order returns H5T_ORDER_ERROR");
   }
   if( type_order == H5T_ORDER_LE )
      order_string = "Little endian byte ordering (0)";
   else if( type_order == H5T_ORDER_BE )
      order_string = "Big endian byte ordering (1)";
   else if( type_order == H5T_ORDER_VAX )
      order_string = "VAX mixed byte ordering (2)";
   return( type_order );
}

// Sets the byte ordering of an atomic datatype.  Inheritance class???
void AtomType::setOrder( H5T_order_t order ) const
{
   // Call C routine to set the byte ordering
   herr_t ret_value = H5Tset_order( id, order );
   if( ret_value < 0 )
   {
      throw DataTypeIException("AtomType::setOrder", "H5Tset_order failed");
   }
}

// Returns the precision of an atomic datatype. 
size_t AtomType::getPrecision() const
{
   size_t num_signi_bits = H5Tget_precision( id );  // C routine

   // returns number of significant bits if successful
   if( num_signi_bits == 0 )
   {
      throw DataTypeIException("AtomType::getPrecision",
		"H5Tget_precision returns invalid number of significant bits");
   }
   return( num_signi_bits );
}

// Sets the precision of an atomic datatype. 
void AtomType::setPrecision( size_t precision ) const
{
   // Call C routine to set the datatype precision
   herr_t ret_value = H5Tset_precision( id, precision );
   if( ret_value < 0 )
   {
      throw DataTypeIException("AtomType::setPrecision", "H5Tset_precision failed");
   }
}

// Retrieves the bit offset of the first significant bit. 
// 12/05/00: due to C API change
//	- return type changed from size_t to int
//	- offset = -1 when failure occurs vs. 0
int AtomType::getOffset() const
{
   int offset = H5Tget_offset( id );  // C routine

   // returns a non-negative offset value if successful
   if( offset == -1 )
   {
      throw DataTypeIException("AtomType::getOffset",
		"H5Tget_offset returns a negative offset value");
   }
   return( offset );
}

// Sets the bit offset of the first significant bit. 
void AtomType::setOffset( size_t offset ) const
{
   // Call C routine to set the bit offset
   herr_t ret_value = H5Tset_offset( id, offset );
   if( ret_value < 0 )
   {
      throw DataTypeIException("AtomType::setOffset", "H5Tset_offset failed");
   }
}

// Retrieves the padding type of the least and most-significant bit padding. 
// these two are for Opaque type
//void AtomType::getPad( H5T_pad_t& lsb, H5T_pad_t& msb ) const
//{
   // Call C routine to get the padding type
   //herr_t ret_value = H5Tget_pad( id, &lsb, &msb );
   //if( ret_value < 0 )
   //{
      //throw DataTypeIException("AtomType::getPad", "H5Tget_pad failed");
   //}
//}

// Sets the least and most-significant bits padding types
//void AtomType::setPad( H5T_pad_t lsb, H5T_pad_t msb ) const
//{
   // Call C routine to set the padding type
   //herr_t ret_value = H5Tset_pad( id, lsb, msb );
   //if( ret_value < 0 )
   //{
      //throw DataTypeIException("AtomType::setPad", "H5Tset_pad failed");
   //}
//}

// This destructor terminates access to the datatype; it calls ~DataType
AtomType::~AtomType() {}

#ifndef H5_NO_NAMESPACE
} // end namespace
#endif
