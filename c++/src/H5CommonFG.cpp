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
#include "H5Group.h"
#include "H5AbstractDs.h"
#include "H5DataSpace.h"
#include "H5DataSet.h"
#include "H5File.h"
#include "H5Alltypes.h"

// Since several compilers do not have support template functions, the
// code in H5templates.h are modified to become the common code defined
// in this file.  The common functions use the hdf5 id that is provided
// by the appropriate objects.
// October 2000

// There are a few comments that are common to most of the functions
// defined in this file so they are listed here.
// - when a failure returned by the C API, the functions will 
//   throw an exception, called File_GroupException, so Group or File can 
//   catch it and throw the appropriate exception to the user's application,
//   i.e., GroupInterfaceException or FileInterfaceException.
// June 2000

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif
// Creates a new group at this location which can be a file or another group.
Group createGroupT( const hid_t loc_id, const string name, size_t size_hint )
{
   // Convert string to C-string
   const char* name_C;
   name_C = name.c_str();  // name_C refers to the contents of name as a C-str

   // Call C routine H5Gcreate to create the named group, giving the 
   // location id which can be a file id or a group id
   hid_t group_id = H5Gcreate( loc_id, name_C, size_hint );

   // If the group id is valid, create and return the Group object
   if( group_id > 0 )
   {
      Group group( group_id );
      return( group );
   }
   else
   {
      throw File_GroupException();
   }
}

// Opens an existing group in a location which can be a file or another group
Group openGroupT( const hid_t loc_id, const string name )
{
   // Convert string to C-string
   const char* name_C;
   name_C = name.c_str();  // name_C refers to the contents of name as a C-str

   // Call C routine H5Gopen to open the named group, giving the 
   // location id which can be a file id or a group id
   hid_t group_id = H5Gopen( loc_id, name_C );

   // If the group id is valid, create and return the Group object
   if( group_id > 0 )
   {
      Group group( group_id );
      return( group );
   }
   else
   {
      throw File_GroupException();
   }
}

// Creates a new dataset at this location.
DataSet createDataSetT( const hid_t loc_id, const string name, const DataType& data_type, const DataSpace& data_space, const DSetCreatPropList& create_plist )
{
   // Convert the dataset's name in C++ string to C-string
   const char* name_C;
   name_C = name.c_str();  // name_C refers to the contents of name as a C-str

   // Obtain identifiers for C API
   hid_t type_id = data_type.getId();
   hid_t space_id = data_space.getId();
   hid_t create_plist_id = create_plist.getId();

   // Call C routine H5Dcreate to create the named dataset
   hid_t dataset_id = H5Dcreate( loc_id, name_C, type_id, space_id, create_plist_id );

   // If the dataset id is valid, create and return the DataSet object
   if( dataset_id > 0 )
   {
      DataSet dataset( dataset_id );
      return( dataset );
   }
   else
   {
      throw File_GroupException();
   }
}

// Opens an existing dataset at this location.
DataSet openDataSetT( const hid_t loc_id, const string name )
{
   // Convert the dataset's name in C++ string to C-string
   const char* name_C;
   name_C = name.c_str();  // name_C refers to the contents of name as a C-str

   // Call C function H5Dopen to open the specified dataset, giving
   // the location id and the dataset's name 
   hid_t dataset_id = H5Dopen( loc_id, name_C );

   // If the dataset id is valid, create and return the DataSet object
   if( dataset_id > 0 )
   {
      DataSet dataset( dataset_id );
      return( dataset );
   }
   else
   {
      throw File_GroupException();
   }
}

// Creates a link of the specified type from new_name to current_name;
// both names are interpreted relative to the specified location id 
void linkT( const hid_t loc_id, H5G_link_t link_type, const string curr_name, const string new_name )
{
   // Convert string to C-string
   const char* curr_name_C, *new_name_C;
   curr_name_C = curr_name.c_str();  // refers to contents of curr_name as a C-str
   new_name_C = new_name.c_str();  // refers to contents of new_name as a C-str

   herr_t ret_value = H5Glink( loc_id, link_type, curr_name_C, new_name_C );
   if( ret_value < 0 )
   {
      throw File_GroupException();
   }
}

// Removes the specified name at this location.
void unlinkT( const hid_t loc_id, const string name )
{
   // Convert string to C-string
   const char* name_C;
   name_C = name.c_str();  // name_C refers to the contents of name as a C-str

   herr_t ret_value = H5Gunlink( loc_id, name_C );
   if( ret_value < 0 )
   {
      throw File_GroupException();
   }
}

// Renames an object at this location.
void moveT( const hid_t loc_id, const string src, const string dst )
{
   // Convert string to C-string
   const char* src_C, *dst_C;
   src_C = src.c_str();  // refers to contents of src as a C-str
   dst_C = dst.c_str();  // refers to contents of dst as a C-str

   herr_t ret_value = H5Gmove( loc_id, src_C, dst_C );
   if( ret_value < 0 )
   {
      throw File_GroupException();
   }
}

// Returns information about an object
void getObjinfoT( const hid_t loc_id, const string name, hbool_t follow_link, H5G_stat_t& statbuf )
{
   // Convert string to C-string
   const char* name_C;
   name_C = name.c_str();  // name_C refers to the contents of name as a C-str

   herr_t ret_value = H5Gget_objinfo( loc_id, name_C, follow_link, &statbuf );
   if( ret_value < 0 )
   {
      throw File_GroupException();
   }
}

// Returns the name of the object that the symbolic link points to.
string getLinkvalT( const hid_t loc_id, const string name, size_t size )
{
   // Convert string to C-string - name_C refers to the contents of name 
   // as a C string
   const char* name_C = name.c_str();

   char* value_C = new char[size+1];  // temporary C-string for C API

   herr_t ret_value = H5Gget_linkval( loc_id, name_C, size, value_C );
   if( ret_value < 0 )
   {
      throw File_GroupException();
   }
   string value = string( value_C );
   delete value_C;
   return( value );
}

// Sets the comment for an object specified by its name
void setCommentT( const hid_t loc_id, const string name, const string comment )
{
   // Convert strings to C-strings
   const char* name_C, *comment_C;
   name_C = name.c_str();  // refers to the contents of name as a C-str
   comment_C = comment.c_str();  // refers to the contents of comment as a C-str
   herr_t ret_value = H5Gset_comment( loc_id, name_C, comment_C );
   if( ret_value < 0 )
   {
      throw File_GroupException();
   }
}

// Retrieves comment for specified object
string getCommentT( const hid_t loc_id, const string name, size_t bufsize )
{
   // Convert string to C-string - name_C refers to the contents of name 
   // as a C string
   const char* name_C = name.c_str();

   // temporary C-string for the object's comment
   char* comment_C = new char[bufsize+1]; 

   herr_t ret_value = H5Gget_comment( loc_id, name_C, bufsize, comment_C );

   // if H5Gget_comment returns SUCCEED, return the string comment
   if( ret_value < 0 )
   {
      throw File_GroupException();
   }
   string comment = string( comment_C );
   delete comment_C;
   return( comment );
}

// Mounts the file 'child' onto this group
void mountT( const hid_t loc_id, const string name, hid_t child_id, PropList& plist )
{
   // Obtain identifiers for C API
   hid_t plist_id = plist.getId();

   // Convert string to C-string
   const char* name_C;
   name_C = name.c_str();  // name_C refers to the contents of name as a C-str

   // Call C routine H5Fmount to do the mouting
   herr_t ret_value = H5Fmount( loc_id, name_C, child_id, plist_id );

   // Raise exception if H5Fmount returns negative value
   if( ret_value < 0 )
   {
      throw File_GroupException();
   }
}

// Unmounts the file named 'name' from this parent group
void unmountT( const hid_t loc_id, const string name )
{
   // Convert string to C-string
   const char* name_C;
   name_C = name.c_str();  // name_C refers to the contents of name as a C-str

   // Call C routine H5Fmount to do the mouting
   herr_t ret_value = H5Funmount( loc_id, name_C );

   // Raise exception if H5Funmount returns negative value
   if( ret_value < 0 )
   {
      throw File_GroupException();
   }
}
#ifndef H5_NO_NAMESPACE
}
#endif
