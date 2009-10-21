/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include <string>

#include "H5Include.h"
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

#include <iostream>
    using std::cerr;
    using std::endl;

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

//--------------------------------------------------------------------------
// Function:	CommonFG::createGroup
///\brief	Creates a new group at this location which can be a file
///		or another group.
///\param	name  - IN: Name of the group to create
///\param	size_hint - IN: Indicates the number of bytes to reserve for
///		the names that will appear in the group
///\return	Group instance
///\exception	H5::FileIException or H5::GroupIException
///\par Description
///		The optional \a size_hint specifies how much file space to
///		reserve for storing the names that will appear in this new
///		group. If a non-positive value is provided for the \a size_hint
///		then a default size is chosen.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
Group CommonFG::createGroup( const char* name, size_t size_hint ) const
{
   // Call C routine H5Gcreate to create the named group, giving the
   // location id which can be a file id or a group id
   hid_t group_id = H5Gcreate( getLocId(), name, size_hint );

   // If the creation of the group failed, throw an exception
   if( group_id < 0 )
   {
      throwException("createGroup", "H5Gcreate failed");
   }

   // No failure, create and return the Group object
   Group group( group_id );
   return( group );
}

//--------------------------------------------------------------------------
// Function:	CommonFG::createGroup
///\brief	This is an overloaded member function, provided for convenience.
///		It differs from the above function in that it takes an
///		\c H5std_string for \a name.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
Group CommonFG::createGroup( const H5std_string& name, size_t size_hint ) const
{
   return( createGroup( name.c_str(), size_hint ));
}

//--------------------------------------------------------------------------
// Function:	CommonFG::openGroup
///\brief	Opens an existing group in a location which can be a file
///		or another group.
///\param	name  - IN: Name of the group to open
///\return	Group instance
///\exception	H5::FileIException or H5::GroupIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
Group CommonFG::openGroup( const char* name ) const
{
   // Call C routine H5Gopen to open the named group, giving the
   // location id which can be a file id or a group id
   hid_t group_id = H5Gopen( getLocId(), name );

   // If the opening of the group failed, throw an exception
   if( group_id < 0 )
   {
      throwException("openGroup", "H5Gopen failed");
   }

   // No failure, create and return the Group object
   Group group( group_id );
   return( group );
}

//--------------------------------------------------------------------------
// Function:	CommonFG::openGroup
///\brief	This is an overloaded member function, provided for convenience.
///		It differs from the above function in that it takes an
///		\c H5std_string for \a name.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
Group CommonFG::openGroup( const H5std_string& name ) const
{
   return( openGroup( name.c_str() ));
}

//--------------------------------------------------------------------------
// Function:	CommonFG::createDataSet
///\brief	Creates a new dataset at this location.
///\param	name  - IN: Name of the dataset to create
///\param	data_type - IN: Datatype of the dataset
///\param	data_space - IN: Dataspace for the dataset
///\param	create_plist - IN: Creation properly list for the dataset
///\return	DataSet instance
///\exception	H5::FileIException or H5::GroupIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
DataSet CommonFG::createDataSet( const char* name, const DataType& data_type, const DataSpace& data_space, const DSetCreatPropList& create_plist ) const
{
   // Obtain identifiers for C API
   hid_t type_id = data_type.getId();
   hid_t space_id = data_space.getId();
   hid_t create_plist_id = create_plist.getId();

   // Call C routine H5Dcreate to create the named dataset
   hid_t dataset_id = H5Dcreate( getLocId(), name, type_id, space_id, create_plist_id );

   // If the creation of the dataset failed, throw an exception
   if( dataset_id < 0 )
   {
      throwException("createDataSet", "H5Dcreate failed");
   }

   // No failure, create and return the DataSet object
   DataSet dataset( dataset_id );
   return( dataset );
}

//--------------------------------------------------------------------------
// Function:	CommonFG::createDataSet
///\brief	This is an overloaded member function, provided for convenience.
///		It differs from the above function in that it takes an
///		\c H5std_string for \a name.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
DataSet CommonFG::createDataSet( const H5std_string& name, const DataType& data_type, const DataSpace& data_space, const DSetCreatPropList& create_plist ) const
{
   return( createDataSet( name.c_str(), data_type, data_space, create_plist ));
}

//--------------------------------------------------------------------------
// Function:	CommonFG::openDataSet
///\brief	Opens an existing dataset at this location.
///\param	name  - IN: Name of the dataset to open
///\return	DataSet instance
///\exception	H5::FileIException or H5::GroupIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
DataSet CommonFG::openDataSet( const char* name ) const
{
   // Call C function H5Dopen to open the specified dataset, giving
   // the location id and the dataset's name
   hid_t dataset_id = H5Dopen( getLocId(), name );

   // If the dataset's opening failed, throw an exception
   if( dataset_id < 0 )
   {
      throwException("openDataSet", "H5Dopen failed");
   }

   // No failure, create and return the DataSet object
   DataSet dataset( dataset_id );
   return( dataset );
}

//--------------------------------------------------------------------------
// Function:	CommonFG::openDataSet
///\brief	This is an overloaded member function, provided for convenience.
///		It differs from the above function in that it takes an
///		\c H5std_string for \a name.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
DataSet CommonFG::openDataSet( const H5std_string& name ) const
{
   return( openDataSet( name.c_str() ));
}

//--------------------------------------------------------------------------
// Function:	CommonFG::link
///\brief	Creates a link of the specified type from \a new_name to
///		\a curr_name.
///\param	link_type  - IN: Link type; possible values are
///		\li \c H5G_LINK_HARD
///		\li \c H5G_LINK_SOFT
///\param	curr_name - IN: Name of the existing object if link is a hard
///		link; can be anything for the soft link
///\param	new_name - IN: New name for the object
///\exception	H5::FileIException or H5::GroupIException
///\par Description
///		Note that both names are interpreted relative to the
///		specified location.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void CommonFG::link( H5G_link_t link_type, const char* curr_name, const char* new_name ) const
{
   herr_t ret_value = H5Glink( getLocId(), link_type, curr_name, new_name );
   if( ret_value < 0 )
   {
      throwException("link", new_name);
      //throwException("link", "H5Glink failed");
   }
}

//--------------------------------------------------------------------------
// Function:	CommonFG::link
///\brief	This is an overloaded member function, provided for convenience.
///		It differs from the above function in that it takes an
///		\c H5std_string for \a curr_name and \a new_name.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void CommonFG::link( H5G_link_t link_type, const H5std_string& curr_name, const H5std_string& new_name ) const
{
   link( link_type, curr_name.c_str(), new_name.c_str() );
}

//--------------------------------------------------------------------------
// Function:	CommonFG::unlink
///\brief	Removes the specified name at this location.
///\param	name  - IN: Name of the object to be removed
///\exception	H5::FileIException or H5::GroupIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void CommonFG::unlink( const char* name ) const
{
   herr_t ret_value = H5Gunlink( getLocId(), name );
   if( ret_value < 0 )
   {
      throwException("unlink", "H5Gunlink failed");
   }
}

//--------------------------------------------------------------------------
// Function:	CommonFG::unlink
///\brief	This is an overloaded member function, provided for convenience.
///		It differs from the above function in that it takes an
///		\c H5std_string for \a name.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void CommonFG::unlink( const H5std_string& name ) const
{
   unlink( name.c_str() );
}

//--------------------------------------------------------------------------
// Function:	CommonFG::move
///\brief	Renames an object at this location.
///\param	src - IN: Object's original name
///\param	dst - IN: Object's new name
///\exception	H5::FileIException or H5::GroupIException
///\note
///		Exercise care in moving groups as it is possible to render
///		data in a file inaccessible with Group::move. Please refer
///		to the Group Interface in the HDF5 User's Guide at:
/// <A HREF="../Groups.html">../Groups.html</A>
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void CommonFG::move( const char* src, const char* dst ) const
{
   herr_t ret_value = H5Gmove( getLocId(), src, dst );
   if( ret_value < 0 )
   {
      throwException("move", "H5Gmove failed");
   }
}

//--------------------------------------------------------------------------
// Function:	CommonFG::move
///\brief	This is an overloaded member function, provided for convenience.
///		It differs from the above function in that it takes an
///		\c H5std_string for \a src and \a dst.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void CommonFG::move( const H5std_string& src, const H5std_string& dst ) const
{
   move( src.c_str(), dst.c_str() );
}

//--------------------------------------------------------------------------
// Function:	CommonFG::getObjinfo
///\brief	Returns information about an object.
///\param	name  - IN: Name of the object
///\param	follow_link - IN: Link flag
///\param	statbuf - OUT: Buffer to return information about the object
///\exception	H5::FileIException or H5::GroupIException
///\par Description
///		For more information, please refer to the C layer Reference
///		Manual at:
/// <A HREF="../RM_H5G.html#Group-GetObjinfo">../RM_H5G.html#Group-GetObjinfo</A>
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void CommonFG::getObjinfo( const char* name, hbool_t follow_link, H5G_stat_t& statbuf ) const
{
   herr_t ret_value = H5Gget_objinfo( getLocId(), name, follow_link, &statbuf );
   if( ret_value < 0 )
   {
      throwException("getObjinfo", "H5Gget_objinfo failed");
   }
}

//--------------------------------------------------------------------------
// Function:	CommonFG::getObjinfo
///\brief	This is an overloaded member function, provided for convenience.
///		It differs from the above function in that it takes an
///		\c H5std_string for \a name.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void CommonFG::getObjinfo( const H5std_string& name, hbool_t follow_link, H5G_stat_t& statbuf ) const
{
   getObjinfo( name.c_str(), follow_link, statbuf );
}

//--------------------------------------------------------------------------
// Function:	CommonFG::getObjinfo
///\brief	This is an overloaded member function, provided for convenience.
///		It differs from the above functions in that it doesn't have
///		the paramemter \a follow_link.
// Programmer	Binh-Minh Ribler - Nov, 2005
//--------------------------------------------------------------------------
void CommonFG::getObjinfo( const char* name, H5G_stat_t& statbuf ) const
{
   herr_t ret_value = H5Gget_objinfo( getLocId(), name, 0, &statbuf );
   if( ret_value < 0 )
   {
      throwException("getObjinfo", "H5Gget_objinfo failed");
   }
}

//--------------------------------------------------------------------------
// Function:	CommonFG::getObjinfo
///\brief	This is an overloaded member function, provided for convenience.
///		It differs from the above function in that it takes an
///		\c H5std_string for \a name.
// Programmer	Binh-Minh Ribler - Nov, 2005
//--------------------------------------------------------------------------
void CommonFG::getObjinfo( const H5std_string& name, H5G_stat_t& statbuf ) const
{
   getObjinfo( name.c_str(), statbuf );
}

//--------------------------------------------------------------------------
// Function:	CommonFG::getLinkval
///\brief	Returns the name of the object that the symbolic link points to.
///\param	name  - IN: Symbolic link to the object
///\param	size - IN: Maximum number of characters of value to be returned
///\return	Name of the object
///\exception	H5::FileIException or H5::GroupIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
H5std_string CommonFG::getLinkval( const char* name, size_t size ) const
{
    char* value_C;
    herr_t ret_value;

    if (size == 0)
    {
	value_C = new char[LINK_BUF_SIZE];
	ret_value = H5Gget_linkval( getLocId(), name, LINK_BUF_SIZE, value_C );
    }
    else
    {
	value_C = new char[size];  // temporary C-string for C API
	ret_value = H5Gget_linkval( getLocId(), name, size, value_C );
    }

    if( ret_value < 0 )
    {
	throwException("getLinkval", "H5Gget_linkval failed");
    }
    H5std_string value = H5std_string(value_C);
    delete []value_C;
    return(value);
}

//--------------------------------------------------------------------------
// Function:	CommonFG::getLinkval
///\brief	This is an overloaded member function, provided for convenience.
///		It differs from the above function in that it takes an
///		\c H5std_string for \a name.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
H5std_string CommonFG::getLinkval( const H5std_string& name, size_t size ) const
{
   return( getLinkval( name.c_str(), size ));
}

//--------------------------------------------------------------------------
// Function:	CommonFG::setComment
///\brief	Sets or resets the comment for an object specified by its name.
///\param	name  - IN: Name of the object
///\param	comment - IN: New comment
///\exception	H5::FileIException or H5::GroupIException
///\par	Description
///		If \a comment is an empty string or a null pointer, the comment
///		message is removed from the object.
///		Comments should be relatively short, null-terminated, ASCII
///		strings.  They can be attached to any object that has an
///		object header, e.g., data sets, groups, named data types,
///		and data spaces, but not symbolic links.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void CommonFG::setComment( const char* name, const char* comment ) const
{
   herr_t ret_value = H5Gset_comment( getLocId(), name, comment );
   if( ret_value < 0 )
   {
      throwException("setComment", "H5Gset_comment failed");
   }
}

//--------------------------------------------------------------------------
// Function:	CommonFG::setComment
///\brief	This is an overloaded member function, provided for convenience.
///		It differs from the above function in that it takes an
///		\c H5std_string for \a name and \a comment.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void CommonFG::setComment( const H5std_string& name, const H5std_string& comment ) const
{
   setComment( name.c_str(), comment.c_str() );
}

//--------------------------------------------------------------------------
// Function:	CommonFG::removeComment
///\brief	Removes the comment from an object specified by its name.
///\param	name  - IN: Name of the object
///\exception	H5::FileIException or H5::GroupIException
// Programmer	Binh-Minh Ribler - May 2005
//--------------------------------------------------------------------------
void CommonFG::removeComment(const char* name) const
{
   herr_t ret_value = H5Gset_comment(getLocId(), name, NULL);
   if( ret_value < 0 )
   {
      throwException("removeComment", "H5Gset_comment failed");
   }
}

//--------------------------------------------------------------------------
// Function:	CommonFG::removeComment
///\brief	This is an overloaded member function, provided for convenience.
///		It differs from the above function in that it takes an
///		\c H5std_string for \a name.
// Programmer	Binh-Minh Ribler - May 2005
//--------------------------------------------------------------------------
void CommonFG::removeComment(const H5std_string& name) const
{
   removeComment (name.c_str());
}

//--------------------------------------------------------------------------
// Function:	CommonFG::getComment
///\brief	Retrieves comment for the specified object.
///\param	name  - IN: Name of the object
///\return	Comment string
///\exception	H5::FileIException or H5::GroupIException
// Programmer	Binh-Minh Ribler - May 2005
//--------------------------------------------------------------------------
H5std_string CommonFG::getComment (const H5std_string& name) const
{
   size_t bufsize = 256;        // anticipating the comment's length
   hid_t loc_id = getLocId();   // temporary variable

   // temporary C-string for the object's comment
   char* comment_C = new char[bufsize+1];
   herr_t ret_value = H5Gget_comment (loc_id, name.c_str(), bufsize, comment_C);

   // if the actual length of the comment is longer than the anticipated
   // value, then call H5Gget_comment again with the correct value
   if (ret_value > bufsize)
   {
	bufsize = ret_value;
	delete []comment_C;
	comment_C = new char[bufsize+1];
	ret_value = H5Gget_comment (loc_id, name.c_str(), bufsize, comment_C);
   }

   // if H5Gget_comment returns SUCCEED, return the string comment,
   // otherwise, throw an exception
   if( ret_value < 0 )
   {
      throwException("getComment", "H5Gget_comment failed");
   }
   H5std_string comment = H5std_string(comment_C);
   delete []comment_C;
   return (comment);
}

//--------------------------------------------------------------------------
// Function:	CommonFG::getComment
///\brief	Retrieves comment for the specified object and its comment's
///		length.
///\param	name  - IN: Name of the object
///\param	bufsize - IN: Length of the comment to retrieve
///\return	Comment string
///\exception	H5::FileIException or H5::GroupIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
H5std_string CommonFG::getComment( const char* name, size_t bufsize ) const
{
   // temporary C-string for the object's comment
   char* comment_C = new char[bufsize+1];

   herr_t ret_value = H5Gget_comment( getLocId(), name, bufsize, comment_C );

   // if H5Gget_comment returns SUCCEED, return the string comment
   if( ret_value < 0 )
   {
      throwException("getComment", "H5Gget_comment failed");
   }
   H5std_string comment = H5std_string(comment_C);
   delete []comment_C;
   return( comment );
}

//--------------------------------------------------------------------------
// Function:	CommonFG::getComment
///\brief	This is an overloaded member function, provided for convenience.
///		It differs from the above function in that it takes an
///		\c H5std_string for \a name.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
H5std_string CommonFG::getComment( const H5std_string& name, size_t bufsize ) const
{
   return( getComment( name.c_str(), bufsize ));
}

//--------------------------------------------------------------------------
// Function:	CommonFG::mount
///\brief	Mounts the file \a child onto this group.
///\param	name  - IN: Name of the group
///\param	child - IN: File to mount
///\param	plist - IN: Property list to use
///\exception	H5::FileIException or H5::GroupIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
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
      throwException("mount", "H5Fmount failed");
   }
}

//--------------------------------------------------------------------------
// Function:	CommonFG::mount
///\brief	This is an overloaded member function, provided for convenience.
///		It differs from the above function in that it takes an
///		\c H5std_string for \a name.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void CommonFG::mount( const H5std_string& name, H5File& child, PropList& plist ) const
{
   mount( name.c_str(), child, plist );
}

//--------------------------------------------------------------------------
// Function:	CommonFG::unmount
///\brief	Unmounts the specified file.
///\param	name  - IN: Name of the file to unmount
///\exception	H5::FileIException or H5::GroupIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void CommonFG::unmount( const char* name ) const
{
   // Call C routine H5Fmount to do the mouting
   herr_t ret_value = H5Funmount( getLocId(), name );

   // Raise exception if H5Funmount returns negative value
   if( ret_value < 0 )
   {
      throwException("unmount", "H5Funmount failed");
   }
}

//--------------------------------------------------------------------------
// Function:	CommonFG::unmount
///\brief	This is an overloaded member function, provided for convenience.
///		It differs from the above function in that it takes an
///		\c H5std_string for \a name.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void CommonFG::unmount( const H5std_string& name ) const
{
   unmount( name.c_str() );
}

//--------------------------------------------------------------------------
// Function:	CommonFG::openDataType
///\brief	Opens the named generic datatype at this location.
///\param	name  - IN: Name of the datatype to open
///\return	DataType instance
///\exception	H5::FileIException or H5::GroupIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
DataType CommonFG::openDataType( const char* name ) const
{
   // Call C function H5Topen to open the named datatype in this group,
   // given either the file or group id
   hid_t type_id = H5Topen(getLocId(), name);

   // If the datatype's opening failed, throw an exception
   if( type_id < 0 )
   {
      throwException("openDataType", "H5Topen failed");
   }
   // No failure, create and return the DataType object
   DataType data_type(type_id);
   return(data_type);
}

//--------------------------------------------------------------------------
// Function:	CommonFG::openDataType
///\brief	This is an overloaded member function, provided for convenience.
///		It differs from the above function in that it takes an
///		\c H5std_string for \a name.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
DataType CommonFG::openDataType( const H5std_string& name ) const
{
   return( openDataType( name.c_str()) );
}

//--------------------------------------------------------------------------
// Function:	CommonFG::openArrayType
///\brief	Opens the named array datatype at this location.
///\param	name  - IN: Name of the array datatype to open
///\return	ArrayType instance
///\exception	H5::FileIException or H5::GroupIException
// Programmer	Binh-Minh Ribler - Jul, 2005
//--------------------------------------------------------------------------
ArrayType CommonFG::openArrayType( const char* name ) const
{
   // Call C function H5Topen to open the named datatype in this group,
   // given either the file or group id
   hid_t type_id = H5Topen(getLocId(), name);

   // If the datatype's opening failed, throw an exception
   if( type_id < 0 )
   {
      throwException("openArrayType", "H5Topen failed");
   }
   // No failure, create and return the ArrayType object
   ArrayType array_type (type_id);
   return(array_type);
}

//--------------------------------------------------------------------------
// Function:	CommonFG::openArrayType
///\brief	This is an overloaded member function, provided for convenience.
///		It differs from the above function in that it takes an
///		\c H5std_string for \a name.
// Programmer	Binh-Minh Ribler - Jul, 2005
//--------------------------------------------------------------------------
ArrayType CommonFG::openArrayType( const H5std_string& name ) const
{
   return( openArrayType( name.c_str()) );
}

//--------------------------------------------------------------------------
// Function:	CommonFG::openCompType
///\brief	Opens the named compound datatype at this location.
///\param	name  - IN: Name of the compound datatype to open
///\return	CompType instance
///\exception	H5::FileIException or H5::GroupIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
CompType CommonFG::openCompType( const char* name ) const
{
   // Call C function H5Topen to open the named datatype in this group,
   // given either the file or group id
   hid_t type_id = H5Topen(getLocId(), name);

   // If the datatype's opening failed, throw an exception
   if( type_id < 0 )
   {
      throwException("openCompType", "H5Topen failed");
   }
   // No failure, create and return the CompType object
   CompType comp_type(type_id);
   return(comp_type);
}

//--------------------------------------------------------------------------
// Function:	CommonFG::openCompType
///\brief	This is an overloaded member function, provided for convenience.
///		It differs from the above function in that it takes an
///		\c H5std_string for \a name.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
CompType CommonFG::openCompType( const H5std_string& name ) const
{
   return( openCompType( name.c_str()) );
}

//--------------------------------------------------------------------------
// Function:	CommonFG::openEnumType
///\brief	Opens the named enumeration datatype at this location.
///\param	name  - IN: Name of the enumeration datatype to open
///\return	EnumType instance
///\exception	H5::FileIException or H5::GroupIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
EnumType CommonFG::openEnumType( const char* name ) const
{
   // Call C function H5Topen to open the named datatype in this group,
   // given either the file or group id
   hid_t type_id = H5Topen(getLocId(), name);

   // If the datatype's opening failed, throw an exception
   if( type_id < 0 )
   {
      throwException("openEnumType", "H5Topen failed");
   }
   // No failure, create and return the EnumType object
   EnumType enum_type(type_id);
   return(enum_type);
}

//--------------------------------------------------------------------------
// Function:	CommonFG::openEnumType
///\brief	This is an overloaded member function, provided for convenience.
///		It differs from the above function in that it takes an
///		\c H5std_string for \a name.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
EnumType CommonFG::openEnumType( const H5std_string& name ) const
{
   return( openEnumType( name.c_str()) );
}

//--------------------------------------------------------------------------
// Function:	CommonFG::openIntType
///\brief	Opens the named integer datatype at this location.
///\param	name  - IN: Name of the integer datatype to open
///\return	IntType instance
///\exception	H5::FileIException or H5::GroupIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
IntType CommonFG::openIntType( const char* name ) const
{
   // Call C function H5Topen to open the named datatype in this group,
   // given either the file or group id
   hid_t type_id = H5Topen(getLocId(), name);

   // If the datatype's opening failed, throw an exception
   if( type_id < 0 )
   {
      throwException("openIntType", "H5Topen failed");
   }
   // No failure, create and return the IntType object
   IntType int_type(type_id);
   return(int_type);
}

//--------------------------------------------------------------------------
// Function:	CommonFG::openIntType
///\brief	This is an overloaded member function, provided for convenience.
///		It differs from the above function in that it takes an
///		\c H5std_string for \a name.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
IntType CommonFG::openIntType( const H5std_string& name ) const
{
   return( openIntType( name.c_str()) );
}

//--------------------------------------------------------------------------
// Function:	CommonFG::openFloatType
///\brief	Opens the named floating-point datatype at this location.
///\param	name  - IN: Name of the floating-point datatype to open
///\return	FloatType instance
///\exception	H5::FileIException or H5::GroupIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
FloatType CommonFG::openFloatType( const char* name ) const
{
   // Call C function H5Topen to open the named datatype in this group,
   // given either the file or group id
   hid_t type_id = H5Topen(getLocId(), name);

   // If the datatype's opening failed, throw an exception
   if( type_id < 0 )
   {
      throwException("openFloatType", "H5Topen failed");
   }
   // No failure, create and return the FloatType object
   FloatType float_type(type_id);
   return(float_type);
}

//--------------------------------------------------------------------------
// Function:	CommonFG::openFloatType
///\brief	This is an overloaded member function, provided for convenience.
///		It differs from the above function in that it takes an
///		\c H5std_string for \a name.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
FloatType CommonFG::openFloatType( const H5std_string& name ) const
{
   return( openFloatType( name.c_str()) );
}

//--------------------------------------------------------------------------
// Function:	CommonFG::openStrType
///\brief	Opens the named string datatype at this location.
///\param	name  - IN: Name of the string datatype to open
///\return	StrType instance
///\exception	H5::FileIException or H5::GroupIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
StrType CommonFG::openStrType( const char* name ) const
{
   // Call C function H5Topen to open the named datatype in this group,
   // given either the file or group id
   hid_t type_id = H5Topen(getLocId(), name);

   // If the datatype's opening failed, throw an exception
   if( type_id < 0 )
   {
      throwException("openStrType", "H5Topen failed");
   }
   // No failure, create and return the StrType object
   StrType str_type(type_id);
   return(str_type);
}

//--------------------------------------------------------------------------
// Function:	CommonFG::openStrType
///\brief	This is an overloaded member function, provided for convenience.
///		It differs from the above function in that it takes an
///		\c H5std_string for \a name.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
StrType CommonFG::openStrType( const H5std_string& name ) const
{
   return( openStrType( name.c_str()) );
}

//--------------------------------------------------------------------------
// Function:	CommonFG::openVarLenType
///\brief	Opens the named variable length datatype at this location.
///\param	name  - IN: Name of the variable length datatype to open
///\return	VarLenType instance
///\exception	H5::FileIException or H5::GroupIException
// Programmer	Binh-Minh Ribler - Jul, 2005
//--------------------------------------------------------------------------
VarLenType CommonFG::openVarLenType( const char* name ) const
{
   // Call C function H5Topen to open the named datatype in this group,
   // given either the file or group id
   hid_t type_id = H5Topen(getLocId(), name);

   // If the datatype's opening failed, throw an exception
   if( type_id < 0 )
   {
      throwException("openVarLenType", "H5Topen failed");
   }
   // No failure, create and return the VarLenType object
   VarLenType varlen_type(type_id);
   return(varlen_type);
}

//--------------------------------------------------------------------------
// Function:	CommonFG::openVarLenType
///\brief	This is an overloaded member function, provided for convenience.
///		It differs from the above function in that it takes an
///		\c H5std_string for \a name.
// Programmer	Binh-Minh Ribler - Jul, 2005
//--------------------------------------------------------------------------
VarLenType CommonFG::openVarLenType( const H5std_string& name ) const
{
   return( openVarLenType( name.c_str()) );
}

//--------------------------------------------------------------------------
// Function:	CommonFG::iterateElems
///\brief	Iterates a user's function over the entries of a group.
///\param	name    - IN    : Name of group to iterate over
///\param	idx     - IN/OUT: Starting (IN) and ending (OUT) entry indices
///\param	op      - IN    : User's function to operate on each entry
///\param	op_data - IN/OUT: Data associated with the operation
///\return	The return value of the first operator that returns non-zero,
///		or zero if all members were processed with no operator
///		returning non-zero.
///\exception	H5::FileIException or H5::GroupIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
int CommonFG::iterateElems( const char* name, int *idx, H5G_iterate_t op , void* op_data )
{
   int ret_value = H5Giterate( getLocId(), name, idx, op, op_data );
   if( ret_value < 0 )
   {
      throwException("iterateElems", "H5Giterate failed");
   }
   return( ret_value );
}

//--------------------------------------------------------------------------
// Function:	CommonFG::iterateElems
///\brief	This is an overloaded member function, provided for convenience.
///		It differs from the above function in that it takes an
///		\c H5std_string for \a name.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
int CommonFG::iterateElems( const H5std_string& name, int *idx, H5G_iterate_t op , void* op_data )
{
   return( iterateElems( name.c_str(), idx, op, op_data ));
}

//--------------------------------------------------------------------------
// Function:	CommonFG::getNumObjs
///\brief	Returns the number of objects in this group.
///\return	Number of objects
///\exception	H5::FileIException or H5::GroupIException
// Programmer	Binh-Minh Ribler - January, 2003
//--------------------------------------------------------------------------
hsize_t CommonFG::getNumObjs() const
{
   hsize_t num_objs;
   herr_t ret_value = H5Gget_num_objs(getLocId(), &num_objs);
   if(ret_value < 0)
   {
      throwException("getNumObjs", "H5Gget_num_objs failed");
   }
   return (num_objs);
}

//--------------------------------------------------------------------------
// Function:	CommonFG::getObjnameByIdx
///\brief	Returns the name of an object in this group, given the
///		object's index.
///\param	idx  -     IN: Transient index of the object
///\return	Object name
///\exception	H5::FileIException or H5::GroupIException
///\par Description
///		The value of idx can be any nonnegative number less than the
///		total number of objects in the group, which is returned by
///		the function \c CommonFG::getNumObjs.  Note that this is a
///		transient index; thus, an object may have a different index
///		each time the group is opened.
// Programmer	Binh-Minh Ribler - Mar, 2005
//--------------------------------------------------------------------------
H5std_string CommonFG::getObjnameByIdx(hsize_t idx) const
{
    // call H5Gget_objname_by_idx with name as NULL to get its length
    ssize_t name_len = H5Gget_objname_by_idx(getLocId(), idx, NULL, 0);
    if(name_len < 0)
    {
      throwException("getObjnameByIdx", "H5Gget_objname_by_idx failed");
    }

    // now, allocate C buffer to get the name
    char* name_C = new char[name_len+1];
    name_len = H5Gget_objname_by_idx(getLocId(), idx, name_C, name_len+1);

    // clean up and return the string
    H5std_string name = H5std_string(name_C);
    delete []name_C;
    return (name);
}

//--------------------------------------------------------------------------
// Function:	CommonFG::getObjnameByIdx
///\brief	Retrieves the name of an object in this group, given the
///		object's index.
///\param	idx  -     IN: Transient index of the object
///\param	name - IN/OUT: Retrieved name of the object
///\param	size -     IN: Length to retrieve
///\return	Actual size of the object name or 0, if object has no name
///\exception	H5::FileIException or H5::GroupIException
///\par Description
///		The value of idx can be any nonnegative number less than the
///		total number of objects in the group, which is returned by
///		the function \c CommonFG::getNumObjs.  Note that this is a
///		transient index; thus, an object may have a different index
///		each time the group is opened.
// Programmer	Binh-Minh Ribler - January, 2003
//--------------------------------------------------------------------------
ssize_t CommonFG::getObjnameByIdx(hsize_t idx, H5std_string& name, size_t size) const
{
   char* name_C = new char[size+1];
   ssize_t name_len = H5Gget_objname_by_idx(getLocId(), idx, name_C, size+1);
   if(name_len < 0)
   {
      throwException("getObjnameByIdx", "H5Gget_objname_by_idx failed");
   }
   name = H5std_string(name_C);
   delete []name_C;
   return (name_len);
}

//--------------------------------------------------------------------------
// Function:	CommonFG::getObjTypeByIdx
///\brief	Returns the type of an object in this group, given the
///		object's index.
///\param	idx - IN: Transient index of the object
///\return	Object type
///\exception	H5::FileIException or H5::GroupIException
// Programmer	Binh-Minh Ribler - January, 2003
//--------------------------------------------------------------------------
H5G_obj_t CommonFG::getObjTypeByIdx(hsize_t idx) const
{
   H5G_obj_t obj_type = H5Gget_objtype_by_idx(getLocId(), idx);
   if (obj_type == H5G_UNKNOWN)
   {
      throwException("getObjTypeByIdx", "H5Gget_objtype_by_idx failed");
   }
   return (obj_type);
}

//--------------------------------------------------------------------------
// Function:	CommonFG::getObjTypeByIdx
///\brief	This is an overloaded member function, provided for convenience.
///		It differs from the above function because it also provides
///		the returned object type in text.
///\param	idx       - IN: Transient index of the object
///\param	type_name - IN: Object type in text
///\return	Object type
///\exception	H5::FileIException or H5::GroupIException
// Programmer	Binh-Minh Ribler - January, 2003
//--------------------------------------------------------------------------
H5G_obj_t CommonFG::getObjTypeByIdx(hsize_t idx, H5std_string& type_name) const
{
   H5G_obj_t obj_type = H5Gget_objtype_by_idx(getLocId(), idx);
   switch (obj_type)
   {
	case H5G_LINK: type_name = H5std_string("symbolic link"); break;
	case H5G_GROUP: type_name = H5std_string("group"); break;
	case H5G_DATASET: type_name = H5std_string("dataset"); break;
	case H5G_TYPE: type_name = H5std_string("datatype"); break;
	case H5G_UNKNOWN:
	default:
   	{
	   throwException("getObjTypeByIdx", "H5Gget_objtype_by_idx failed");
	}
   }
   return (obj_type);
}
//--------------------------------------------------------------------------
// Function:	CommonFG default constructor
///\brief	Default constructor.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
CommonFG::CommonFG() {}

//--------------------------------------------------------------------------
// Function:	CommonFG destructor
///\brief	Noop destructor.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
CommonFG::~CommonFG() {}

#ifndef H5_NO_NAMESPACE
}
#endif
