#include <string>

#include "H5Include.h"
#include "H5RefCounter.h"
#include "H5Exception.h"
#include "H5IdComponent.h"
#include "H5Idtemplates.h"
#include "H5PropList.h"
#include "H5Object.h"
#include "H5DataType.h"
#include "H5DataSpace.h"
#include "H5AbstractDs.h"
#include "H5Attribute.h"

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

// userAttrOpWrpr simply interfaces between the user's function and the
// C library function H5Aiterate; used to resolve the different prototype 
// problem.  May be moved to Iterator later.
extern "C" herr_t userAttrOpWrpr( hid_t loc_id, const char* attr_name, void* op_data )
{
   string s_attr_name = string( attr_name );
#ifdef NO_STATIC_CAST
   UserData4Aiterate* myData = (UserData4Aiterate *) op_data;
#else
   UserData4Aiterate* myData = static_cast <UserData4Aiterate *> (op_data);
#endif
   myData->op( *myData->object, s_attr_name, myData->opData );
   return 0;
}

// Default constructor - set id to 0 by default here but may be set
// to a valid HDF5 id, if any, by a subclass constructor.
H5Object::H5Object() : IdComponent() {}

// Constructs an object from an existing HDF5 id
H5Object::H5Object( const hid_t object_id ) : IdComponent( object_id ) {}

// Copy constructor: makes a copy of the original H5Object instance
H5Object::H5Object( const H5Object& original ) : IdComponent( original ) {}

// Creates an attribute for a group, dataset, or named datatype.
Attribute H5Object::createAttribute( const char* name, const DataType& data_type, const DataSpace& data_space, const PropList& create_plist ) const
{
   hid_t type_id = data_type.getId();
   hid_t space_id = data_space.getId();
   hid_t plist_id = create_plist.getId();
   hid_t attr_id = H5Acreate( id, name, type_id, space_id, plist_id );

   // If the attribute id is valid, create and return the Attribute object
   if( attr_id > 0 )
   {
      Attribute attr( attr_id );
      return( attr );
   }
   else
   {
      throw AttributeIException();
   }
}

// Creates an attribute for a group, dataset, or named datatype.
Attribute H5Object::createAttribute( const string& name, const DataType& data_type, const DataSpace& data_space, const PropList& create_plist ) const
{
   return( createAttribute( name.c_str(), data_type, data_space, create_plist ));
}

// Opens an attribute given its name; name is given as char*
Attribute H5Object::openAttribute( const char* name ) const
{
   hid_t attr_id = H5Aopen_name( id, name );
   if( attr_id > 0 )
   {
      Attribute attr( attr_id );
      return( attr );
   }
   else
   {
      throw AttributeIException();
   }
}

// Opens an attribute given its name; name is given as string
Attribute H5Object::openAttribute( const string& name ) const
{
   return( openAttribute( name.c_str()) );
}

// Opens an attribute given its index.
Attribute H5Object::openAttribute( const unsigned int idx ) const
{
   hid_t attr_id = H5Aopen_idx( id, idx );
   if( attr_id > 0 )
   {
      Attribute attr( attr_id );
      return( attr );
   }
   else
   {
      throw AttributeIException();
   }
}

// Iterates a user's function over all the attributes of the dataset
int H5Object::iterateAttrs( attr_operator_t user_op, unsigned * idx, void *op_data )
{
   // store the user's function and data
   UserData4Aiterate* userData = new UserData4Aiterate;
   userData->opData = op_data;
   userData->idx = idx;
   userData->op = user_op;
   userData->object = this;

   // call the C library routine H5Aiterate to iterate the attributes
   int ret_value = H5Aiterate( id, idx, userAttrOpWrpr, (void *) userData );
   // release memory
   delete userData;

   if( ret_value >= 0 )
      return( ret_value );
   else  // raise exception when H5Aiterate returns a negative value
   {
      throw AttributeIException();
   }
}

// Determines the number of attributes attached to 
int H5Object::getNumAttrs() const
{
   int num_attrs = H5Aget_num_attrs( id );
   if( num_attrs < 0 )
   {
      throw AttributeIException();
   }
   else
      return( num_attrs );
}

// Removes the named attribute from this object.
void H5Object::removeAttr( const char* name ) const
{
   herr_t ret_value = H5Adelete( id, name );
   if( ret_value < 0 )
   {
      throw AttributeIException();
   }
}
void H5Object::removeAttr( const string& name ) const
{
   removeAttr( name.c_str() );
}

// Flushes all buffers associated with a file to disk.
void H5Object::flush(H5F_scope_t scope ) const
{
   herr_t ret_value = H5Fflush( id, scope );
   if( ret_value < 0 )
   {
      throw FileIException();
   }
}

// each subclass' destructor calls the template function resetIdComponent()
// to reset the corresponding IdComponent object and close the HDF5 object
// where appropriate.
H5Object::~H5Object() {}

#ifndef H5_NO_NAMESPACE
} // end namespace
#endif
