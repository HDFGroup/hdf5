/*
These functions provide code that are common to both H5File and Group.
Some of the member functions of these two classes call a common function
and provide it a file or group id to perform a task that can be done on
either an H5File or Group instance. 10/31/00

The name of the functions ends with a T because these functions were
template functions until it was realized that more than one of our
supported platforms have not supported template functions.
*/

#ifndef _CommonFG_H
#define _CommonFG_H

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

// Creates a new group at this location which can be a file or another group.
Group createGroupT( const hid_t loc_id, const string name, size_t size_hint );

// Opens an existing group in a location which can be a file or another group
Group openGroupT( const hid_t loc_id, const string name );

// Creates a new dataset at this location.
DataSet createDataSetT( const hid_t loc_id, const string name, const DataType& data_type, const DataSpace& data_space, const DSetCreatPropList& create_plist );

// Opens an existing dataset at this location.
DataSet openDataSetT( const hid_t loc_id, const string name );

// Creates a link of the specified type from new_name to current_name;
// both names are interpreted relative to the specified location id
void linkT( const hid_t loc_id, H5G_link_t link_type, const string curr_name, const string new_name );

// Removes the specified name at this location.
void unlinkT( const hid_t loc_id, const string name );

// Renames an object at this location.
void moveT( const hid_t loc_id, const string src, const string dst );

// Returns information about an object
void getObjinfoT( const hid_t loc_id, const string name, hbool_t follow_link, H5G_stat_t& statbuf );

// Returns the name of the object that the symbolic link points to.
string getLinkvalT( const hid_t loc_id, const string name, size_t size );

// Sets the comment for an object specified by its name
void setCommentT( const hid_t loc_id, const string name, const string comment );

// Retrieves comment for specified object
string getCommentT( const hid_t loc_id, const string name, size_t bufsize );

// Mounts the file 'child' onto this group
void mountT( const hid_t loc_id, const string name, hid_t child_id, PropList& plist );

// Unmounts the file named 'name' from this parent group
void unmountT( const hid_t loc_id, const string name );
#ifndef H5_NO_NAMESPACE
}
#endif
#endif

