#include <string>

#include "H5Include.h"
#include "H5RefCounter.h"
#include "H5Exception.h"
#include "H5IdComponent.h"
#include "H5Idtemplates.h"
#include "H5PropList.h"
#include "H5Object.h"
#include "H5AbstractDs.h"
#include "H5FaccProp.h"
#include "H5FcreatProp.h"
#include "H5DcreatProp.h"
#include "H5DxferProp.h"
#include "H5DataSpace.h"
#include "H5DataSet.h"
#include "H5CommonFG.h"
#include "H5Group.h"
#include "H5File.h"
#include "H5Alltypes.h"

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

// Default constructor
Group::Group() : H5Object() {}

// Copy constructor: makes a copy of the original Group object 
Group::Group( const Group& original ) : H5Object( original ) {}

// Creates a new group in this group using the common function
// provided in FGtemplates.h.
Group Group::createGroup( const string& name, size_t size_hint )
{
   return( createGroup( name.c_str(), size_hint ));
}
Group Group::createGroup( const char* name, size_t size_hint )
{
   try {
      Group group = createGroupT( id, name, size_hint );
      return( group );
   }
   catch( File_GroupException error )
   {
      throw GroupIException();
   }
}

// Creates a copy of an existing Group using its id
Group::Group( const hid_t group_id ) : H5Object( group_id ) {};

// Opens an existing group in this group using the common function
// provided in FGtemplates.h.
Group Group::openGroup( const string& name )
{
   return( openGroup( name.c_str() ));
}
Group Group::openGroup( const char* name )
{
   try {
      Group group = openGroupT( id, name );
      return( group );
   }
   catch( File_GroupException error )
   {
      throw GroupIException();
   }
}

// Creates a dataset in this group using the common function
// provided in FGtemplates.h  
DataSet Group::createDataSet( const string& name, const DataType& data_type, const DataSpace& data_space, const DSetCreatPropList& create_plist )
{
   return( createDataSet( name.c_str(), data_type, data_space, create_plist ));
}
DataSet Group::createDataSet( const char* name, const DataType& data_type, const DataSpace& data_space, const DSetCreatPropList& create_plist )
{
   try {
      DataSet dataset = createDataSetT( id, name, data_type, data_space, create_plist );
      return( dataset );
   }
   catch( File_GroupException error )
   {
      throw GroupIException();
   }
}

// Opens a dataset in this group using the common function
// provided in FGtemplates.h  
DataSet Group::openDataSet( const string& name )
{
   return( openDataSet( name.c_str() ));
}
DataSet Group::openDataSet( const char* name )
{
   try {
      DataSet dataset = openDataSetT( id, name );
      return( dataset );
   }
   catch( File_GroupException error )
   {
      throw GroupIException();
   }
}

// This private member function calls the C API H5Topen to open the 
// named datatype and returns the datatype's identifier.  The function 
// is used by the functions openXxxType's below for opening the sub-types
hid_t Group::p_openDataType( const char* name ) const
{ 
   // Call C function H5Topen to open the named datatype in this group,
   // giving the group id 
   hid_t datatype_id = H5Topen( id, name );
   
   // If the datatype id is valid, return it, otherwise, throw an exception.
   if( datatype_id > 0 ) 
      return( datatype_id );
   else
   { 
      throw GroupIException();
   }  
}  

//
// The following member functions use the private function
// p_openDataType to open a named datatype in this group
//

// Opens the named generic datatype in this group.
DataType Group::openDataType( const string& name ) const
{
   return( openDataType( name.c_str()) );
}
DataType Group::openDataType( const char* name ) const
{
   DataType data_type( p_openDataType( name ));
   return( data_type );
}

// Opens the named enumeration datatype in this group.
EnumType Group::openEnumType( const string& name ) const
{
   return( openEnumType( name.c_str()) );
}  
EnumType Group::openEnumType( const char* name ) const
{
   EnumType enum_type( p_openDataType( name ));
   return( enum_type );
}  

// Opens the named compound datatype in this group.
CompType Group::openCompType( const string& name ) const
{
   return( openCompType( name.c_str()) );
}
CompType Group::openCompType( const char* name ) const
{
   CompType comp_type( p_openDataType( name ));
   return( comp_type );
}

// Opens the named integer datatype in this group.
IntType Group::openIntType( const string& name ) const
{  
   return( openIntType( name.c_str()) );
}
IntType Group::openIntType( const char* name ) const
{  
   IntType int_type( p_openDataType( name ));
   return( int_type );
}

// Opens the named floating-point datatype in this group.
FloatType Group::openFloatType( const string& name ) const
{  
   return( openFloatType( name.c_str()) );
}
FloatType Group::openFloatType( const char* name ) const
{  
   FloatType float_type( p_openDataType( name ));
   return( float_type );
}

// Opens the named string datatype of this group
StrType Group::openStrType( const string& name ) const
{
   return( openStrType( name.c_str()) );
}
StrType Group::openStrType( const char* name ) const
{
   StrType str_type( p_openDataType( name ));
   return( str_type );
}

// Iterates a user's function over the entries of a group.
int Group::iterateElems( const string& name, int *idx, H5G_iterate_t op , void *op_data )
{
   return( iterateElems( name.c_str(), idx, op, op_data ));
}
int Group::iterateElems( const char* name, int *idx, H5G_iterate_t op , void *op_data )
{
   int ret_value = H5Giterate( id, name, idx, op, op_data );
   if( ret_value >= 0 )
      return( ret_value );
   else  // raise exception when H5Aiterate returns a negative value
   {
      throw GroupIException();
   }
}

// Creates a link of the specified type from new_name to current_name;
// both names are interpreted relative to this group.
void Group::link( H5G_link_t link_type, const string& curr_name, const string& new_name )
{
   link( link_type, curr_name.c_str(), new_name.c_str() );
}
void Group::link( H5G_link_t link_type, const char* curr_name, const char* new_name )
{
   try {
      linkT( id, link_type, curr_name, new_name );
   }
   catch( File_GroupException error )
   {
      throw GroupIException();
   }
}

// Removes the specified name from this group.
void Group::unlink( const string& name )
{
   unlink( name.c_str() );
}
void Group::unlink( const char* name )
{
   try {
      unlinkT( id, name );
   }
   catch( File_GroupException error )
   {
      throw GroupIException();
   }
}

// Renames an object from this group.
void Group::move( const string& src, const string& dst )
{
   move( src.c_str(), dst.c_str() );
}
void Group::move( const char* src, const char* dst )
{
   try {
      moveT( id, src, dst );
   }
   catch( File_GroupException error )
   {
      throw GroupIException();
   }
}

// Retrieves information about an object.
void Group::getObjinfo( const string& name, hbool_t follow_link, H5G_stat_t& statbuf )
{
   getObjinfo( name.c_str(), follow_link, statbuf );
}
void Group::getObjinfo( const char* name, hbool_t follow_link, H5G_stat_t& statbuf )
{
   try {
      getObjinfoT( id, name, follow_link, statbuf );
   }
   catch( File_GroupException error )
   {
      throw GroupIException();
   }
}

// Returns the name of the object that the symbolic link points to.
string Group::getLinkval( const string& name, size_t size )
{
   return( getLinkval( name.c_str(), size ));
}
string Group::getLinkval( const char* name, size_t size )
{
   try {
      string value = getLinkvalT( id, name, size );
      return( value );
   }
   catch( File_GroupException error )
   {
      throw GroupIException();
   }
}

// Sets comment for an object specified by its name.
void Group::setComment( const string& name, const string& comment )
{
   setComment( name.c_str(), comment );
}
void Group::setComment( const char* name, const char* comment )
{
   try {
      setCommentT( id, name, comment );
   }
   catch( File_GroupException error )
   {
      throw GroupIException();
   }
}

// Retrieves the comment of an object specified by its name
string Group::getComment( const string& name, size_t bufsize )
{
   return( getComment( name.c_str(), bufsize ));
}
string Group::getComment( const char* name, size_t bufsize )
{
   try {
      string comment = getCommentT( id, name, bufsize );
      return( comment );
   }
   catch( File_GroupException error )
   {
      throw GroupIException();
   }
}

// Mounts the file 'child' onto this group.
void Group::mount( const string& name, H5File& child, PropList& plist )
{
   mount( name.c_str(), child, plist );
}
void Group::mount( const char* name, H5File& child, PropList& plist )
{
   try {
      mountT( id, name, child.getId(), plist );
   }
   catch( File_GroupException error )
   {
      throw GroupIException();
   }
}

// Unmounts the file named 'name' from this parent group.
void Group::unmount( const string& name )
{
   unmount( name.c_str() );
}
void Group::unmount( const char* name )
{
   try {
      unmountT( id, name );
   }
   catch( File_GroupException error )
   {
      throw GroupIException();
   }
}

// Calls the C API H5Gclose to close this group.  Used by IdComponent::reset
void Group::p_close() const
{
   herr_t ret_value = H5Gclose( id );
   if( ret_value < 0 )
   {
      throw GroupIException();
   }
}

// The destructor of this instance calls IdComponent::reset to
// reset its identifier - no longer true
// Older compilers (baldric) don't support template member functions
// and IdComponent::reset is one; so at this time, the resetId is not
// a member function so it can be template to work around that problem.
Group::~Group()
{  
   // The group id will be closed properly
   resetIdComponent( this );
}  

#ifndef H5_NO_NAMESPACE
} // end namespace
#endif
