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
#include "H5Exception.h"
#include "H5RefCounter.h"
#include "H5IdComponent.h"
#include "H5Idtemplates.h"
#include "H5PropList.h"
#include "H5Object.h"
#include "H5AbstractDs.h"
#include "H5Attribute.h"
#include "H5DataType.h"
#include "H5DataSpace.h"

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

// Copy constructor: makes a copy of the original object; simply invokes
// the base class copy constructor.
Attribute::Attribute( const Attribute& original ) : AbstractDs( original ) {}

// Creates a copy of an existing attribute using an id
Attribute::Attribute( const hid_t attr_id ) : AbstractDs( attr_id ) {}

// Writes data to this attribute.
void Attribute::write( const DataType& mem_type, const void *buf ) const
{
   herr_t ret_value = H5Awrite( id, mem_type.getId(), buf );
   if( ret_value < 0 )
   {
      throw AttributeIException("Attribute::write", "H5Awrite failed");
   }
}

// Reads data from this attribute.
void Attribute::read( const DataType& mem_type, void *buf ) const
{
   herr_t ret_value = H5Aread( id, mem_type.getId(), buf );
   if( ret_value < 0 )
   {
      throw AttributeIException("Attribute::read", "H5Aread  failed");
   }
}

// Gets a copy of the dataspace for this attribute.
DataSpace Attribute::getSpace() const
{
   // Calls C function H5Aget_space to get the id of the dataspace
   hid_t dataspace_id = H5Aget_space( id );

   // If the dataspace id is valid, create and return the DataSpace object
   if( dataspace_id > 0 )
   {
      DataSpace dataspace( dataspace_id );
      return( dataspace );
   }
   else
   {
      throw AttributeIException("Attribute::getSpace", "H5Aget_space failed");
   }
}

// This private member function calls the C API to get the generic datatype
// of the datatype that is used by this attribute.  This function is used
// by the overloaded functions getDataType defined in AbstractDs for the 
// generic datatype and specific sub-types.
hid_t Attribute::p_getType() const
{
   hid_t type_id = H5Aget_type( id );
   if( type_id > 0 )
      return( type_id );
   else
   {
      throw AttributeIException(NULL, "H5Aget_type failed");
   }
}

// Gets the name of this attribute, returning its length.
ssize_t Attribute::getName( size_t buf_size, string& attr_name ) const
{
   char* name_C = new char[buf_size+1];  // temporary C-string for C API

   // Calls C routine H5Aget_name to get the name of the attribute
   ssize_t name_size = H5Aget_name( id, buf_size, name_C );

   // If H5Aget_name returns a negative value, raise an exception,
   if( name_size < 0 )
   {
      throw AttributeIException("Attribute::getName", "H5Aget_name failed");
   }
   // otherwise, convert the C string attribute name and return 
   attr_name = string( name_C );
   delete name_C;
   return( name_size );
}

// Gets the name of this attribute, returning the name, not the length.
string Attribute::getName( size_t buf_size ) const
{
   string attr_name;
   ssize_t name_size = getName( buf_size, attr_name );
   return( attr_name ); 
   // let caller catch exception if any
}

// This private function calls the C API H5Aclose to close this attribute.
// Used by the IdComponent::reset.
void Attribute::p_close() const
{
   herr_t ret_value = H5Aclose( id );
   if( ret_value < 0 )
   {
      throw AttributeIException(NULL, "H5Aclose failed");
   }
}

// The destructor of this instance calls IdComponent::reset to
// reset its identifier - no longer true
// Older compilers (baldric) don't support template member functions
// and IdComponent::reset is one; so at this time, the resetId is not
// a member function so it can be template to work around that problem.
Attribute::~Attribute()
{
   // The attribute id will be closed properly
    try {
        resetIdComponent( this ); }
    catch (Exception close_error) { // thrown by p_close
        throw AttributeIException("Attribute::~Attribute", close_error.getDetailMsg());
    }
}

#ifndef H5_NO_NAMESPACE
} // end namespace
#endif
