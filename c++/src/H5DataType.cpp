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
#include "H5Idtemplates.h"
#include "H5PropList.h"
#include "H5Object.h"
#include "H5DataType.h"
#include "H5AtomType.h"
#include "H5PredType.h"
#include "H5private.h"

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

// Constructor creates a copy of an existing DataType using its id.
// 'predefined' is default to false; when a default datatype is 
// created, this argument is set to true so H5Tclose will not be 
// called on it later.
DataType::DataType( const hid_t existing_id, bool predefined ) : H5Object( existing_id ), is_predtype( predefined ) {
}

// Creates a datatype given its class and size
DataType::DataType( const H5T_class_t type_class, size_t size ) : H5Object(), is_predtype( false )
{
   // Call C routine to create the new datatype
   id = H5Tcreate( type_class, size );
   if( id <= 0 )
   {
      throw DataTypeIException("DataType constructor", "H5Tcreate failed");
   }
}

// Default constructor
DataType::DataType() : H5Object(), is_predtype( false )
{
}

// Copy constructor: makes a copy of this DataType object.
DataType::DataType( const DataType& original ) : H5Object( original  )
{
   is_predtype = original.is_predtype; // copy data member from original
}

// Copies an existing datatype to this datatype object
void DataType::copy( const DataType& like_type )
{
   // reset the identifier of this instance, H5Tclose will be called 
   // if needed 
    try {
        resetIdComponent( this ); }
    catch (Exception close_error) { // thrown by p_close
        throw DataTypeIException("DataType::copy", close_error.getDetailMsg());
    }

   // call C routine to copy the datatype
   id = H5Tcopy( like_type.getId() );

   // new reference counter for this id
   ref_count = new RefCounter;

   if( id <= 0 )
   {
      throw DataTypeIException("DataType::copy", "H5Tcopy failed");
   }
}

// Makes a copy of the type on the right hand side and stores the new
// id in the left hand side object.  
DataType& DataType::operator=( const DataType& rhs )
{
   copy(rhs);
   return(*this);
}

// Determines whether two datatypes refer to the same actual datatype.
bool DataType::operator==(const DataType& compared_type ) const
{
   // Call C routine H5Tequal to determines whether two datatype 
   // identifiers refer to the same datatype
   htri_t ret_value = H5Tequal( id, compared_type.getId() );
   if( ret_value > 0 )
      return true;
   else if( ret_value == 0 )
      return false;
   else
   {
      throw DataTypeIException("DataType::operator==", 
		"H5Tequal returns negative value");
   }
}

// Operates a user's function on each attribute of an object - commented
// out because it should use the one from H5Object; need to check 
// the parameter list???  - work in progress
//int DataType::iterate( unsigned * idx, H5A_operator_t op, void *op_data )
//{
//}

// Creates a new variable-length datatype - Note: should use inheritance -
// work in progress
//DataType DataType::vlenCreate( const DataType& base_type )
//{
//}

// Commits a transient datatype to a file, creating a new named datatype
void DataType::commit( H5Object& loc, const string& name ) const
{
   commit( loc, name.c_str() );
}
void DataType::commit( H5Object& loc, const char* name ) const
{
   hid_t loc_id = loc.getId(); // get location id for C API

   // Call C routine to commit the transient datatype 
   herr_t ret_value = H5Tcommit( loc_id, name, id );
   if( ret_value < 0 )
   {
      throw DataTypeIException("DataType::commit", "H5Tcommit failed");
   }
}

// Determines whether a datatype is a named type or a transient type. 
bool DataType::committed() const
{
   // Call C function to determine if a datatype is a named one
   htri_t committed = H5Tcommitted( id );
   if( committed > 0 )
      return true;
   else if( committed == 0 )
      return false;
   else
   {
      throw DataTypeIException("DataType::committed", "H5Tcommitted return negative value");
   }
}

// Finds a conversion function.
H5T_conv_t DataType::find( const DataType& dest, H5T_cdata_t **pcdata ) const
{
   // Call C routine to find the conversion function
   H5T_conv_t func = H5Tfind( id, dest.getId(), pcdata );
   if( func == NULL )
   {
      throw DataTypeIException("DataType::find", "H5Tfind returns a NULL function");
   }
   return( func );
}

// Converts data from between specified datatypes. 
void DataType::convert( const DataType& dest, size_t nelmts, void *buf, void *background, PropList& plist ) const
{
   // Get identifiers for C API
   hid_t dest_id = dest.getId();
   hid_t plist_id = plist.getId();

   // Call C routine H5Tconvert to convert the data
   herr_t ret_value;
   ret_value = H5Tconvert( id, dest_id, nelmts, buf, background, plist_id );
   if( ret_value < 0 )
   {
      throw DataTypeIException("DataType::convert", "H5Tconvert failed");
   }
}

// Sets the overflow handler to a specified function. 
void DataType::setOverflow( H5T_overflow_t func ) const
{
   // Call C routine H5Tset_overflow to set the overflow handler
   herr_t ret_value = H5Tset_overflow( func );
   if( ret_value < 0 )
   {
      throw DataTypeIException("DataType::setOverflow", "H5Tset_overflow failed");
   }
}

// Returns a pointer to the current global overflow function. 
H5T_overflow_t DataType::getOverflow(void) const
{
   return( H5Tget_overflow());  // C routine
   // NULL can be returned as well
}

// Locks a datatype. 
void DataType::lock() const
{
   // Call C routine to lock the datatype
   herr_t ret_value = H5Tlock( id );
   if( ret_value < 0 )
   {
      throw DataTypeIException("DataType::lock", "H5Tlock failed");
   }
}

// Returns the datatype class identifier. 
H5T_class_t DataType::getClass() const
{
   H5T_class_t type_class = H5Tget_class( id );

   // Return datatype class identifier if successful
   if( type_class == H5T_NO_CLASS )
   {
      throw DataTypeIException("DataType::getClass", 
		"H5Tget_class returns H5T_NO_CLASS");
   }
   return( type_class );
}

// Returns the size of a datatype. 
size_t DataType::getSize() const
{
   // Call C routine to get the datatype size
   size_t type_size = H5Tget_size( id );
   if( type_size <= 0 ) // valid data types are never zero size
   {
      throw DataTypeIException("DataType::getSize", 
		"H5Tget_size returns invalid datatype size");
   }
   return( type_size );
}

// Returns the base datatype from which a datatype is derived. 
// - just for DataType?
DataType DataType::getSuper() const
{
   // Call C routine to get the base datatype from which the specified
   // datatype is derived. 
   hid_t base_type_id = H5Tget_super( id );

   // If H5Tget_super returns a valid datatype id, create and return
   // the base type, otherwise, raise exception
   if( base_type_id > 0 )
   {
      DataType base_type( base_type_id );
      return( base_type );
   }
   else
   {
      throw DataTypeIException("DataType::getSuper", "H5Tget_super failed");
   }
}

// Registers the specified conversion function. 
void DataType::registerFunc( H5T_pers_t pers, const string& name, const DataType& dest, H5T_conv_t func ) const
{
   registerFunc( pers, name.c_str(), dest, func );
}
void DataType::registerFunc( H5T_pers_t pers, const char* name, const DataType& dest, H5T_conv_t func ) const
{
   hid_t dest_id = dest.getId();  // get id of the destination datatype

   // Call C routine H5Tregister to register the conversion function
   herr_t ret_value = H5Tregister( pers, name, id, dest_id, func );
   if( ret_value < 0 )
   {
      throw DataTypeIException("DataType::registerFunc", "H5Tregister failed");
   }
}

// Removes a conversion function from all conversion paths. 
void DataType::unregister( H5T_pers_t pers, const string& name, const DataType& dest, H5T_conv_t func ) const
{
   unregister( pers, name.c_str(), dest, func );
}
void DataType::unregister( H5T_pers_t pers, const char* name, const DataType& dest, H5T_conv_t func ) const
{
   hid_t dest_id = dest.getId();  // get id of the dest datatype for C API

   // Call C routine H5Tunregister to remove the conversion function 
   herr_t ret_value = H5Tunregister( pers, name, id, dest_id, func );
   if( ret_value < 0 )
   {
      throw DataTypeIException("DataType::unregister", "H5Tunregister failed");
   }
}

// Tags an opaque datatype. 
void DataType::setTag( const string& tag ) const
{
   setTag( tag.c_str());
}
void DataType::setTag( const char* tag ) const
{
   // Call C routine H5Tset_tag to tag an opaque datatype. 
   herr_t ret_value = H5Tset_tag( id, tag );
   if( ret_value < 0 )
   {
      throw DataTypeIException("DataType::setTag", "H5Tset_tag failed");
   }
}

// Gets the tag associated with an opaque datatype. 
string DataType::getTag() const
{
   char* tag_Cstr = H5Tget_tag( id );

   // if the tag C-string returned is not NULL, convert it to C++ string
   // and return it, otherwise, raise an exception
   if( tag_Cstr != NULL )
   {
      string tag = string(tag_Cstr); // convert C string to string object
	HDfree(tag_Cstr); // free the C string 
      return (tag); // return the tag 
   }
   else
   {
      throw DataTypeIException("DataType::getTag", 
		"H5Tget_tag returns NULL for tag");
   }
}

// This private function calls the C API H5Tclose to close this datatype.
// Used by H5Object::p_reset.
void DataType::p_close() const
{
   // If this datatype is not a predefined type, call H5Tclose on it.
   if( is_predtype == false )
   {
      herr_t ret_value = H5Tclose( id );
      if( ret_value < 0 )
      {
         throw DataTypeIException(NULL, "H5Tclose failed");
      }
   }
}

// The destructor of this instance calls IdComponent::reset to
// reset its identifier - no longer true
// Older compilers (baldric) don't support template member functions
// and IdComponent::reset is one; so at this time, the resetId is not
// a member function so it can be template to work around that problem.
DataType::~DataType()
{  
   // The datatype id will be closed properly
    try {
        resetIdComponent( this ); }
    catch (Exception close_error) { // thrown by p_close
        throw DataTypeIException("DataType::~DataType", close_error.getDetailMsg());
    }
}  

#ifndef H5_NO_NAMESPACE
} // end namespace
#endif
