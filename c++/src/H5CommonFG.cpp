#include <string>

#include "H5Include.h"
#include "H5RefCounter.h"
#include "H5Exception.h"
#include "H5IdComponent.h"
#include "H5PropList.h"
#include "H5Object.h"
#include "H5FaccProp.h"
#include "H5FcreatProp.h"
#include "H5DxferProp.h"
#include "H5DcreatProp.h"
#include "H5CommonFG.h"
#include "H5Group.h"
#include "H5AbstractDs.h"
#include "H5DataSpace.h"
#include "H5DataSet.h"
#include "H5File.h"
#include "H5Alltypes.h"

// There are a few comments that are common to most of the functions
// defined in this file so they are listed here.
// - getLocId is called by all functions, that call a C API, to get
//   the location id, which can be either a file id or a group id.
//   This function is pure virtual and it's up to H5File and Group
//   to call the right getId() - although, as the structure of the
//   library at this time, getId() is basically the IdComponent::getId()
// - when a failure returned by the C API, the functions will call
//   throwException, which is a pure virtual function and is implemented
//   by H5File to throw a FileIException and by Group to throw a
//   GroupIException.
// December 2000

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

// Creates a new group at this location which can be a file or another group.
Group CommonFG::createGroup( const string& name, size_t size_hint ) const
{
   return( createGroup( name.c_str(), size_hint ));
}

Group CommonFG::createGroup( const char* name, size_t size_hint ) const
{
   // Call C routine H5Gcreate to create the named group, giving the 
   // location id which can be a file id or a group id
   hid_t group_id = H5Gcreate( getLocId(), name, size_hint );

   // If the group id is valid, create and return the Group object
   if( group_id > 0 )
   {
      Group group( group_id );
      return( group );
   }
   else
   {
      //throw File_GroupException();
      throwException();
   }
}

// Opens an existing group in a location which can be a file or another group
Group CommonFG::openGroup( const string& name ) const
{
   return( openGroup( name.c_str() ));
}
Group CommonFG::openGroup( const char* name ) const
{
   // Call C routine H5Gopen to open the named group, giving the 
   // location id which can be a file id or a group id
   hid_t group_id = H5Gopen( getLocId(), name );

   // If the group id is valid, create and return the Group object
   if( group_id > 0 )
   {
      Group group( group_id );
      return( group );
   }
   else
   {
      //throw File_GroupException();
      throwException();
   }
}

// Creates a new dataset at this location.
DataSet CommonFG::createDataSet( const string& name, const DataType& data_type, const DataSpace& data_space, const DSetCreatPropList& create_plist ) const
{
   return( createDataSet( name.c_str(), data_type, data_space, create_plist ));
}
DataSet CommonFG::createDataSet( const char* name, const DataType& data_type, const DataSpace& data_space, const DSetCreatPropList& create_plist ) const
{
   // Obtain identifiers for C API
   hid_t type_id = data_type.getId();
   hid_t space_id = data_space.getId();
   hid_t create_plist_id = create_plist.getId();

   // Call C routine H5Dcreate to create the named dataset
   hid_t dataset_id = H5Dcreate( getLocId(), name, type_id, space_id, create_plist_id );

   // If the dataset id is valid, create and return the DataSet object
   if( dataset_id > 0 )
   {
      DataSet dataset( dataset_id );
      return( dataset );
   }
   else
   {
      //throw File_GroupException();
      throwException();
   }
}

// Opens an existing dataset at this location.
DataSet CommonFG::openDataSet( const string& name ) const
{
   return( openDataSet( name.c_str() ));
}
DataSet CommonFG::openDataSet( const char* name ) const
{
   // Call C function H5Dopen to open the specified dataset, giving
   // the location id and the dataset's name 
   hid_t dataset_id = H5Dopen( getLocId(), name );

   // If the dataset id is valid, create and return the DataSet object
   if( dataset_id > 0 )
   {
      DataSet dataset( dataset_id );
      return( dataset );
   }
   else
   {
      //throw File_GroupException();
      throwException();
   }
}

// Creates a link of the specified type from new_name to current_name;
// both names are interpreted relative to the specified location id 
void CommonFG::link( H5G_link_t link_type, const string& curr_name, const string& new_name ) const
{
   link( link_type, curr_name.c_str(), new_name.c_str() );
}
void CommonFG::link( H5G_link_t link_type, const char* curr_name, const char* new_name ) const
{
   herr_t ret_value = H5Glink( getLocId(), link_type, curr_name, new_name );
   if( ret_value < 0 )
   {
      //throw File_GroupException();
      throwException();
   }
}

// Removes the specified name at this location.
void CommonFG::unlink( const string& name ) const
{
   unlink( name.c_str() );
}
void CommonFG::unlink( const char* name ) const
{
   herr_t ret_value = H5Gunlink( getLocId(), name );
   if( ret_value < 0 )
   {
      //throw File_GroupException();
      throwException();
   }
}

// Renames an object at this location.
void CommonFG::move( const string& src, const string& dst ) const
{
   move( src.c_str(), dst.c_str() );
}
void CommonFG::move( const char* src, const char* dst ) const
{
   herr_t ret_value = H5Gmove( getLocId(), src, dst );
   if( ret_value < 0 )
   {
      //throw File_GroupException();
      throwException();
   }
}

// Returns information about an object
void CommonFG::getObjinfo( const string& name, hbool_t follow_link, H5G_stat_t& statbuf ) const
{
   getObjinfo( name.c_str(), follow_link, statbuf );
}
void CommonFG::getObjinfo( const char* name, hbool_t follow_link, H5G_stat_t& statbuf ) const
{
   herr_t ret_value = H5Gget_objinfo( getLocId(), name, follow_link, &statbuf );
   if( ret_value < 0 )
   {
      //throw File_GroupException();
      throwException();
   }
}

// Returns the name of the object that the symbolic link points to.
string CommonFG::getLinkval( const string& name, size_t size ) const
{
   return( getLinkval( name.c_str(), size ));
}
string CommonFG::getLinkval( const char* name, size_t size ) const
{
   char* value_C = new char[size+1];  // temporary C-string for C API

   herr_t ret_value = H5Gget_linkval( getLocId(), name, size, value_C );
   if( ret_value < 0 )
   {
      //throw File_GroupException();
      throwException();
   }
   string value = string( value_C );
   delete value_C;
   return( value );
}

// Sets the comment for an object specified by its name
void CommonFG::setComment( const string& name, const string& comment ) const
{
   setComment( name.c_str(), comment.c_str() );
}
void CommonFG::setComment( const char* name, const char* comment ) const
{
   herr_t ret_value = H5Gset_comment( getLocId(), name, comment );
   if( ret_value < 0 )
   {
      //throw File_GroupException();
      throwException();
   }
}

// Retrieves comment for specified object
string CommonFG::getComment( const string& name, size_t bufsize ) const
{
   return( getComment( name.c_str(), bufsize ));
}
string CommonFG::getComment( const char* name, size_t bufsize ) const
{
   // temporary C-string for the object's comment
   char* comment_C = new char[bufsize+1]; 

   herr_t ret_value = H5Gget_comment( getLocId(), name, bufsize, comment_C );

   // if H5Gget_comment returns SUCCEED, return the string comment
   if( ret_value < 0 )
   {
      //throw File_GroupException();
      throwException();
   }
   string comment = string( comment_C );
   delete comment_C;
   return( comment );
}

// Mounts the file 'child' onto this group
void CommonFG::mount( const string& name, H5File& child, PropList& plist ) const
{
   mount( name.c_str(), child, plist );
}
void CommonFG::mount( const char* name, H5File& child, PropList& plist ) const
{
   // Obtain identifiers for C API
   hid_t plist_id = plist.getId();
   hid_t child_id = child.getId();

   // Call C routine H5Fmount to do the mouting
   herr_t ret_value = H5Fmount( getLocId(), name, child_id, plist_id );

   // Raise exception if H5Fmount returns negative value
   if( ret_value < 0 )
   {
      //throw File_GroupException();
      throwException();
   }
}

// Unmounts the file named 'name' from this parent group
void CommonFG::unmount( const string& name ) const
{
   unmount( name.c_str() );
}
void CommonFG::unmount( const char* name ) const
{
   // Call C routine H5Fmount to do the mouting
   herr_t ret_value = H5Funmount( getLocId(), name );

   // Raise exception if H5Funmount returns negative value
   if( ret_value < 0 )
   {
      //throw File_GroupException();
      throwException();
   }
}

// This private member function calls the C API H5Topen to open the 
// named datatype and returns the datatype's identifier.  The function 
// is used by the functions openXxxType's below for opening the sub-types
hid_t CommonFG::p_openDataType( const char* name ) const
{ 
   // Call C function H5Topen to open the named datatype in this group,
   // giving the group id 
   hid_t datatype_id = H5Topen( getLocId(), name );
   
   // If the datatype id is valid, return it, otherwise, throw an exception.
   if( datatype_id > 0 ) 
      return( datatype_id );
   else
   { 
      //throw GroupIException();
      throwException();
   }  
}  

//
// The following member functions use the private function
// p_openDataType to open a named datatype in this location
//

// Opens the named generic datatype in this group.
DataType CommonFG::openDataType( const string& name ) const
{
   return( openDataType( name.c_str()) );
}
DataType CommonFG::openDataType( const char* name ) const
{
   DataType data_type( p_openDataType( name ));
   return( data_type );
}

// Opens the named enumeration datatype in this group.
EnumType CommonFG::openEnumType( const string& name ) const
{
   return( openEnumType( name.c_str()) );
}  
EnumType CommonFG::openEnumType( const char* name ) const
{
   EnumType enum_type( p_openDataType( name ));
   return( enum_type );
}  

// Opens the named compound datatype in this group.
CompType CommonFG::openCompType( const string& name ) const
{
   return( openCompType( name.c_str()) );
}
CompType CommonFG::openCompType( const char* name ) const
{
   CompType comp_type( p_openDataType( name ));
   return( comp_type );
}

// Opens the named integer datatype in this group.
IntType CommonFG::openIntType( const string& name ) const
{  
   return( openIntType( name.c_str()) );
}
IntType CommonFG::openIntType( const char* name ) const
{  
   IntType int_type( p_openDataType( name ));
   return( int_type );
}

// Opens the named floating-point datatype in this group.
FloatType CommonFG::openFloatType( const string& name ) const
{  
   return( openFloatType( name.c_str()) );
}
FloatType CommonFG::openFloatType( const char* name ) const
{  
   FloatType float_type( p_openDataType( name ));
   return( float_type );
}

// Opens the named string datatype of this group
StrType CommonFG::openStrType( const string& name ) const
{
   return( openStrType( name.c_str()) );
}
StrType CommonFG::openStrType( const char* name ) const
{
   StrType str_type( p_openDataType( name ));
   return( str_type );
}

// Iterates a user's function over the entries of a group.
int CommonFG::iterateElems( const string& name, int *idx, H5G_iterate_t op , void* op_data )
{
   return( iterateElems( name.c_str(), idx, op, op_data ));
}
int CommonFG::iterateElems( const char* name, int *idx, H5G_iterate_t op , void* op_data )
{
   int ret_value = H5Giterate( getLocId(), name, idx, op, op_data );
   if( ret_value >= 0 )
      return( ret_value );
   else  // raise exception when H5Aiterate returns a negative value
   {
      throwException();
   }
}

CommonFG::CommonFG() 
{ // do nothing 
}

CommonFG::~CommonFG() 
{ // do nothing 
}

#ifndef H5_NO_NAMESPACE
}
#endif
