#include <string>

#include "H5Include.h"
#include "H5RefCounter.h"
#include "H5Exception.h"
#include "H5IdComponent.h"
#include "H5Idtemplates.h"
#include "H5PropList.h"
#include "H5Object.h"
#include "H5FaccProp.h"
#include "H5FcreatProp.h"
#include "H5DxferProp.h"
#include "H5DcreatProp.h"
#include "H5Group.h"
#include "H5AbstractDs.h"
#include "H5DataSpace.h"
#include "H5DataSet.h"
#include "H5File.h"
#include "H5Alltypes.h"
#include "H5CommonFG.h"

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

// Creates or opens an HDF5 file depending on the parameter flags.
H5File::H5File( const string& name, unsigned int flags, const FileCreatPropList& create_plist, const FileAccPropList& access_plist ) : IdComponent()
{
   getFile( name.c_str(), flags, create_plist, access_plist );
}

H5File::H5File( const char* name, unsigned int flags, const FileCreatPropList& create_plist, const FileAccPropList& access_plist ) : IdComponent()
{
   getFile( name, flags, create_plist, access_plist );
}

// This function is private and contains common code between the 
// constructors taking a string or a char*
void H5File::getFile( const char* name, unsigned int flags, const FileCreatPropList& create_plist, const FileAccPropList& access_plist )
{
   // These bits only set for creation, so if any of them are set,
   // create the file.
   if( flags & (H5F_ACC_EXCL|H5F_ACC_TRUNC|H5F_ACC_DEBUG ))
   {
      hid_t create_plist_id = create_plist.getId();
      hid_t access_plist_id = access_plist.getId();
      id = H5Fcreate( name, flags, create_plist_id, access_plist_id );
   }
   // Open the file if none of the bits above are set.
   else
   {
      // use create_plist for access plist because of the default argument
      hid_t access_plist_id = create_plist.getId();
      id = H5Fopen( name, flags, access_plist_id );
   }

   if( id <= 0 )  // throw an exception when open/create fail
   {
      throw FileIException();
   }
}

// Copy constructor: makes a copy of the original H5File object.
H5File::H5File( const H5File& original ) : IdComponent( original ) {}

// Determines whether a file specified by its name in HDF5 format
bool H5File::isHdf5(const string& name ) 
{
   return( isHdf5( name.c_str()) );
}
bool H5File::isHdf5(const char* name ) 
{
   // Calls C routine H5Fis_hdf5 to determine whether the file is in 
   // HDF5 format.  It returns positive value, 0, or negative value
   htri_t ret_value = H5Fis_hdf5( name );
   if( ret_value > 0 )
      return true;
   else if( ret_value == 0 )
      return false;
   else // Raise exception when H5Fis_hdf5 returns a negative value 
   {
      throw FileIException();
   }
}

// Reopens this file
void H5File::reopen()
{
   // reset the identifier of this H5File - send 'this' in so that
   // H5Fclose can be called appropriately
   resetIdComponent( this );

   // call C routine to reopen the file - Note: not sure about this
   // does id need to be closed later?  which id to be the parameter?
   id = H5Freopen( id );
   if( id <= 0 ) // Raise exception when H5Freopen returns a neg value
   {
      throw FileIException();
   }
}

// Creates a new group in this file using the template function provided
// in FGtemplates.h  
Group H5File::createGroup( const string& name, size_t size_hint ) const
{
   return( createGroup( name.c_str(), size_hint ));
}

Group H5File::createGroup( const char* name, size_t size_hint ) const
{
   try {
      Group group = createGroupT( id, name, size_hint );
      return( group );
   }
   catch( File_GroupException error )
   {
      throw FileIException();
   }
}

// Opens an existing group in this file using the template function provided
// in FGtemplates.h  
Group H5File::openGroup( const string& name ) const
{
   return( openGroup( name.c_str() ));
}

Group H5File::openGroup( const char* name ) const
{
   try {
      Group group = openGroupT( id, name );
      return( group );
   }
   catch( File_GroupException error )
   {
      throw FileIException();
   }
}

// Creates a dataset in this file using the template function
// provided in FGtemplates.h  
DataSet H5File::createDataSet( const string& name, const DataType& data_type, const DataSpace& data_space, const DSetCreatPropList& create_plist ) const
{
   return( createDataSet( name.c_str(), data_type, data_space, create_plist ));
}

DataSet H5File::createDataSet( const char* name, const DataType& data_type, const DataSpace& data_space, const DSetCreatPropList& create_plist ) const
{
   try {
      DataSet dataset = createDataSetT( id, name, data_type, data_space, create_plist );
      return( dataset );
   }
   catch( File_GroupException error )
   {
      throw FileIException();
   }
}

// Opens an existing dataset in this file using the template function
// provided in FGtemplates.h  
DataSet H5File::openDataSet( const string& name ) const
{
   return( openDataSet( name.c_str() ));
}

DataSet H5File::openDataSet( const char* name ) const
{
   try {
      DataSet dataset = openDataSetT( id, name );
      return( dataset );
   }
   catch( File_GroupException error )
   {
      throw FileIException();
   }
}

// This private member function calls the C API H5Topen to open the
// named datatype in this file, and returns the datatype's identifier.  
// The function is used by the functions openXxxType's below for 
// opening the sub-types
hid_t H5File::p_openDataType( const char* name ) const
{
   // Call C function H5Topen to open the named datatype in this group,
   // giving the group id
   hid_t datatype_id = H5Topen( id, name );

   // If the datatype id is valid, return it, otherwise, throw an exception.
   if( datatype_id > 0 )
      return( datatype_id );
   else
   {
      throw FileIException();
   }
}

//
// The following member functions use the private function
// p_openDataType to open a named datatype in this group
//

// Opens the named generic datatype in this group.
DataType H5File::openDataType( const string& name ) const
{
   return( openDataType( name.c_str()) );
}
DataType H5File::openDataType( const char* name ) const
{
   DataType data_type( p_openDataType( name ));
   return( data_type );
}

// Opens the named enumeration datatype in this group.
EnumType H5File::openEnumType( const string& name ) const
{
   return( openEnumType( name.c_str()) );
}
EnumType H5File::openEnumType( const char* name ) const
{
   EnumType enum_type( p_openDataType( name ));
   return( enum_type );
}

// Opens the named compound datatype in this group.
CompType H5File::openCompType( const string& name ) const
{
   return( openCompType( name.c_str()) );
}
CompType H5File::openCompType( const char* name ) const
{
   CompType comp_type( p_openDataType( name ));
   return( comp_type );
}

// Opens the named integer datatype in this group.
IntType H5File::openIntType( const string& name ) const
{
   return( openIntType( name.c_str()) );
}
IntType H5File::openIntType( const char* name ) const
{ 
   IntType int_type( p_openDataType( name ));
   return( int_type );
}

// Opens the named floating-point datatype in this group.
FloatType H5File::openFloatType( const string& name ) const
{
   return( openFloatType( name.c_str()) );
}
FloatType H5File::openFloatType( const char* name ) const
{
   FloatType float_type( p_openDataType( name ));
   return( float_type );
}

// Opens the named string datatype of this group
StrType H5File::openStrType( const string& name ) const
{
   return( openStrType( name.c_str()) );
}
StrType H5File::openStrType( const char* name ) const
{
   StrType str_type( p_openDataType( name ));
   return( str_type );
}

// Returns the creation property list of this file
FileCreatPropList H5File::getCreatePlist() const
{
   hid_t create_plist_id = H5Fget_create_plist( id );

   // if H5Fget_create_plist returns a valid id, create and return
   // the FileCreatPropList object for this property list
   if( create_plist_id > 0 )
   {
      FileCreatPropList create_plist( create_plist_id );
      return( create_plist );
   }
   else
   {
      throw FileIException();
   }
}

// Returns the access property list of this file
FileAccPropList H5File::getAccessPlist() const
{
   hid_t access_plist_id = H5Fget_access_plist( id );

   // if H5Fget_access_plist returns a valid id, create and return
   // the FileAccPropList object for this property list
   if( access_plist_id > 0 )
   {
      FileAccPropList access_plist( access_plist_id );
      return access_plist;
   }
   else // Raise an exception
   {
      throw FileIException();
   }
}

// Creates a link of the specified type from new_name to current_name;
// both names are interpreted relative to this file
void H5File::link( H5G_link_t link_type, const string& curr_name, const string& new_name ) const
{
   link( link_type, curr_name.c_str(), new_name.c_str() );
}

void H5File::link( H5G_link_t link_type, const char* curr_name, const char* new_name ) const
{
   try {
      linkT( id, link_type, curr_name, new_name );
   }
   catch( File_GroupException error ) {
      throw FileIException();
   }
}

// Removes the specified name from this file.
void H5File::unlink( const string& name ) const
{
   unlink( name.c_str());
}

void H5File::unlink( const char* name ) const
{
   try {
      unlinkT( id, name );
   }
   catch( File_GroupException error ) {
      throw FileIException();
   }
}

// Renames an object from this file.
void H5File::move( const string& src, const string& dst ) const
{
   move( src.c_str(), dst.c_str());
}

void H5File::move( const char* src, const char* dst ) const
{
   try {
      moveT( id, src, dst );
   }
   catch( File_GroupException error ) {
      throw FileIException();
   }
}

// Returns information about an object
void H5File::getObjinfo( const string& name, hbool_t follow_link, H5G_stat_t& statbuf ) const
{
   getObjinfo( name, follow_link, statbuf );
}

void H5File::getObjinfo( const char* name, hbool_t follow_link, H5G_stat_t& statbuf ) const
{
   try {
      getObjinfoT( id, name, follow_link, statbuf );
   }
   catch( File_GroupException error ) {
      throw FileIException();
   }
}

// Returns the name of the object that the symbolic link points to.
string H5File::getLinkval( const string& name, size_t size ) const
{
   return( getLinkval( name.c_str(), size ));
}

string H5File::getLinkval( const char* name, size_t size ) const
{
   try {
      string value = getLinkvalT( id, name, size );
      return( value );
   }
   catch( File_GroupException error ) {
      throw FileIException();
   }
}

// Sets comment for specified object.
void H5File::setComment( const string& name, const string& comment ) const
{
   setComment( name.c_str(), comment.c_str());
}

void H5File::setComment( const char* name, const char* comment ) const
{
   try {
      setCommentT( id, name, comment );
   }
   catch( File_GroupException error ) {
      throw FileIException();
   }
}

// Retrieves comment for specified object
string H5File::getComment( const string& name, size_t bufsize ) const
{
   return( getComment( name.c_str(), bufsize ));
}

string H5File::getComment( const char* name, size_t bufsize ) const
{
   try {
      string comment = getCommentT( id, name, bufsize );
      return( comment );
   }
   catch( File_GroupException error ) {
      throw FileIException();
   }
}

// Mounts the file 'child' onto this file
void H5File::mount( const string& name, H5File& child, PropList& mount_plist ) const
{
   mount( name.c_str(), child, mount_plist );
}

void H5File::mount( const char* name, H5File& child, PropList& mount_plist ) const
{
   try {
      mountT( id, name, child.getId(), mount_plist );
   }
   catch( File_GroupException error ) {
      throw FileIException();
   }
}

// Unmounts the file named 'name' from this parent file
void H5File::unmount( const string& name ) const
{
   unmount( name.c_str() );
}

void H5File::unmount( const char* name ) const
{
   try {
      unmountT( id, name );
   }
   catch( File_GroupException error ) {
      throw FileIException();
   }
}

// Calls the C API H5Fclose to close this file.  Used by IdComponent::reset
void H5File::p_close() const
{
   herr_t ret_value = H5Fclose( id );
   if( ret_value < 0 )
   {
      throw FileIException();
   }
}

// The destructor of this instance calls IdComponent::reset to
// reset its identifier - no longer true
// Older compilers (baldric) don't support template member functions
// and IdComponent::reset is one; so at this time, the resetId is not
// a member function so it can be template to work around that problem.
H5File::~H5File() 
{  
   // The HDF5 file id will be closed properly
   resetIdComponent( this );
}  

#ifndef H5_NO_NAMESPACE
} // end namespace
#endif
