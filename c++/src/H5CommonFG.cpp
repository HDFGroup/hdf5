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

//--------------------------------------------------------------------------
// Function:	CommonFG::createGroup
///\brief	Creates a new group at this location which can be a file 
///		or another group.
///\param	name  - IN: Name of the group
///\param	value - IN: Size to reserve
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
   if( group_id <= 0 )
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
///		\c std::string for \a name.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
Group CommonFG::createGroup( const string& name, size_t size_hint ) const
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
   if( group_id <= 0 )
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
///		\c std::string for \a name.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
Group CommonFG::openGroup( const string& name ) const
{
   return( openGroup( name.c_str() ));
}

//--------------------------------------------------------------------------
// Function:	CommonFG::createDataSet
///\brief	Creates a new dataset at this location.
///\param	name  - IN: Name of the dataset to create
///\param	data_type - IN: 
///\param	data_space - IN: 
///\param	create_plist - IN: 
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
   if( dataset_id <= 0 )
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
///		\c std::string for \a name.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
DataSet CommonFG::createDataSet( const string& name, const DataType& data_type, const DataSpace& data_space, const DSetCreatPropList& create_plist ) const
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
   if( dataset_id <= 0 )
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
///		\c std::string for \a name.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
DataSet CommonFG::openDataSet( const string& name ) const
{
   return( openDataSet( name.c_str() ));
}

// Creates a link of the specified type from new_name to current_name;
//--------------------------------------------------------------------------
// Function:	CommonFG::link
///\brief	Creates a link of the specified type from \a new_name to 
///		\a curr_name;
///\param	link_type  - IN: 
///\param	curr_name - IN: 
///\param	new_name - IN: 
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
      throwException("link", "H5Glink failed");
   }
}

//--------------------------------------------------------------------------
// Function:	CommonFG::link
///\brief	This is an overloaded member function, provided for convenience.
///		It differs from the above function in that it takes an 
///		\c std::string for \a curr_name and \a new_name.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void CommonFG::link( H5G_link_t link_type, const string& curr_name, const string& new_name ) const
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
///		\c std::string for \a name.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void CommonFG::unlink( const string& name ) const
{
   unlink( name.c_str() );
}

//--------------------------------------------------------------------------
// Function:	CommonFG::move
///\brief	Renames an object at this location.
///\param	src - IN: 
///\param	dst - IN: 
///\exception	H5::FileIException or H5::GroupIException
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
///		\c std::string for \a src and \a dst.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void CommonFG::move( const string& src, const string& dst ) const
{
   move( src.c_str(), dst.c_str() );
}

//--------------------------------------------------------------------------
// Function:	CommonFG::getObjinfo
///\brief	Returns information about an object.
///\param	name  - IN: Name of the object
///\param	follow_link - IN: 
///\param	statbuf - IN: 
///\exception	H5::FileIException or H5::GroupIException
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
///		\c std::string for \a name.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void CommonFG::getObjinfo( const string& name, hbool_t follow_link, H5G_stat_t& statbuf ) const
{
   getObjinfo( name.c_str(), follow_link, statbuf );
}

//--------------------------------------------------------------------------
// Function:	CommonFG::getLinkval
///\brief	Returns the name of the object that the symbolic link points to.
///\param	name  - IN: 
///\param	size - IN: 
///\return	Name of the object
///\exception	H5::FileIException or H5::GroupIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
string CommonFG::getLinkval( const char* name, size_t size ) const
{
   char* value_C = new char[size+1];  // temporary C-string for C API

   herr_t ret_value = H5Gget_linkval( getLocId(), name, size, value_C );
   if( ret_value < 0 )
   {
      throwException("getLinkval", "H5Gget_linkval failed");
   }
   string value = string( value_C );
   delete value_C;
   return( value );
}

//--------------------------------------------------------------------------
// Function:	CommonFG::getLinkval
///\brief	This is an overloaded member function, provided for convenience.
///		It differs from the above function in that it takes an 
///		\c std::string for \a name.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
string CommonFG::getLinkval( const string& name, size_t size ) const
{
   return( getLinkval( name.c_str(), size ));
}

//--------------------------------------------------------------------------
// Function:	CommonFG::setComment
///\brief	Sets the comment for an object specified by its name.
///\param	name  - IN: 
///\param	comment - IN: 
///\exception	H5::FileIException or H5::GroupIException
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
///		\c std::string for \a name and \a comment.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void CommonFG::setComment( const string& name, const string& comment ) const
{
   setComment( name.c_str(), comment.c_str() );
}

//--------------------------------------------------------------------------
// Function:	CommonFG::getComment
///\brief	Retrieves comment for the specified object.
///\param	name  - IN: Name of the object
///\param	bufsize - IN: 
///\return	Comment string
///\exception	H5::FileIException or H5::GroupIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
string CommonFG::getComment( const char* name, size_t bufsize ) const
{
   // temporary C-string for the object's comment
   char* comment_C = new char[bufsize+1]; 

   herr_t ret_value = H5Gget_comment( getLocId(), name, bufsize, comment_C );

   // if H5Gget_comment returns SUCCEED, return the string comment
   if( ret_value < 0 )
   {
      throwException("getComment", "H5Gget_comment failed");
   }
   string comment = string( comment_C );
   delete comment_C;
   return( comment );
}

//--------------------------------------------------------------------------
// Function:	CommonFG::getComment
///\brief	This is an overloaded member function, provided for convenience.
///		It differs from the above function in that it takes an 
///		\c std::string for \a name.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
string CommonFG::getComment( const string& name, size_t bufsize ) const
{
   return( getComment( name.c_str(), bufsize ));
}

//--------------------------------------------------------------------------
// Function:	CommonFG::mount
///\brief	Mounts the file 'child' onto this group.
///\param	name  - IN: 
///\param	child - IN: 
///\param	plist - IN: 
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
///		\c std::string for \a name.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void CommonFG::mount( const string& name, H5File& child, PropList& plist ) const
{
   mount( name.c_str(), child, plist );
}

//--------------------------------------------------------------------------
// Function:	CommonFG::unmount
///\brief	Unmounts the file named 'name' from this parent group.
///\param	name  - IN: 
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
///		\c std::string for \a name.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void CommonFG::unmount( const string& name ) const
{
   unmount( name.c_str() );
}

// This private member function calls the C API H5Topen to open the 
// named datatype and returns the datatype's identifier.  The function 
// is used by the functions openXxxType's below for opening the sub-types
hid_t CommonFG::p_openDataType( const char* name ) const
{ 
   // Call C function H5Topen to open the named datatype in this group,
   // giving either the file or group id 
   hid_t datatype_id = H5Topen( getLocId(), name );
   
   // If the datatype's opening failed, throw an exception
   if( datatype_id <= 0 ) 
   { 
      throwException("openDataType", "H5Topen failed");
   }  

   // No failure, return the datatype id
   return( datatype_id );
}  

//
// The following member functions use the private function
// p_openDataType to open a named datatype in this location
//

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
   DataType data_type( p_openDataType( name ));
   return( data_type );
}

//--------------------------------------------------------------------------
// Function:	CommonFG::openDataType
///\brief	This is an overloaded member function, provided for convenience.
///		It differs from the above function in that it takes an 
///		\c std::string for \a name.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
DataType CommonFG::openDataType( const string& name ) const
{
   return( openDataType( name.c_str()) );
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
   EnumType enum_type( p_openDataType( name ));
   return( enum_type );
}  

//--------------------------------------------------------------------------
// Function:	CommonFG::openEnumType
///\brief	This is an overloaded member function, provided for convenience.
///		It differs from the above function in that it takes an 
///		\c std::string for \a name.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
EnumType CommonFG::openEnumType( const string& name ) const
{
   return( openEnumType( name.c_str()) );
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
   CompType comp_type( p_openDataType( name ));
   return( comp_type );
}

//--------------------------------------------------------------------------
// Function:	CommonFG::openCompType
///\brief	This is an overloaded member function, provided for convenience.
///		It differs from the above function in that it takes an 
///		\c std::string for \a name.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
CompType CommonFG::openCompType( const string& name ) const
{
   return( openCompType( name.c_str()) );
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
   IntType int_type( p_openDataType( name ));
   return( int_type );
}

//--------------------------------------------------------------------------
// Function:	CommonFG::openIntType
///\brief	This is an overloaded member function, provided for convenience.
///		It differs from the above function in that it takes an 
///		\c std::string for \a name.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
IntType CommonFG::openIntType( const string& name ) const
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
   FloatType float_type( p_openDataType( name ));
   return( float_type );
}

//--------------------------------------------------------------------------
// Function:	CommonFG::openFloatType
///\brief	This is an overloaded member function, provided for convenience.
///		It differs from the above function in that it takes an 
///		\c std::string for \a name.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
FloatType CommonFG::openFloatType( const string& name ) const
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
   StrType str_type( p_openDataType( name ));
   return( str_type );
}

//--------------------------------------------------------------------------
// Function:	CommonFG::openStrType
///\brief	This is an overloaded member function, provided for convenience.
///		It differs from the above function in that it takes an 
///		\c std::string for \a name.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
StrType CommonFG::openStrType( const string& name ) const
{
   return( openStrType( name.c_str()) );
}

//--------------------------------------------------------------------------
// Function:	CommonFG::iterateElems
///\brief	Iterates a user's function over the entries of a group.
///\param	name  - IN: 
///\param	idx - IN: 
///\param	op - IN: 
///\param	op_data - IN: 
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
///		\c std::string for \a name.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
int CommonFG::iterateElems( const string& name, int *idx, H5G_iterate_t op , void* op_data )
{
   return( iterateElems( name.c_str(), idx, op, op_data ));
}

//--------------------------------------------------------------------------
// Function:	CommonFG default constructor
///\brief	Default constructor.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
CommonFG::CommonFG() {}

//--------------------------------------------------------------------------
// Function:	CommonFG destructor
///\brief	Properly terminates access to this object.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
CommonFG::~CommonFG() {}

#ifndef H5_NO_NAMESPACE
}
#endif
