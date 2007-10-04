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
#include "H5DcreatProp.h"
#include "H5CommonFG.h"
#include "H5DataType.h"
#include "H5DataSpace.h"
#include "H5AbstractDs.h"
#include "H5Attribute.h"

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

#ifndef DOXYGEN_SHOULD_SKIP_THIS
// userAttrOpWrpr simply interfaces between the user's function and the
// C library function H5Aiterate2; used to resolve the different prototype
// problem.  May be moved to Iterator later.
extern "C" herr_t userAttrOpWrpr(hid_t loc_id, const char *attr_name,
    const H5A_info_t *ainfo, void *op_data)
{
   H5std_string s_attr_name = H5std_string( attr_name );
#ifdef NO_STATIC_CAST
   UserData4Aiterate* myData = (UserData4Aiterate *) op_data;
#else
   UserData4Aiterate* myData = static_cast <UserData4Aiterate *> (op_data);
#endif
   myData->op( *myData->object, s_attr_name, myData->opData );
   return 0;
}

//--------------------------------------------------------------------------
// Function:	H5Object default constructor (protected)
// Description
//		The id is set by IdComponent() but subclass constructor will
//		set it to a valid HDF5 id.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
H5Object::H5Object() : IdComponent(0) {}

//--------------------------------------------------------------------------
// Function:	H5Object overloaded constructor (protected)
// Purpose	Creates an H5Object object using the id of an existing HDF5
// 		object.
// Parameters	object_id - IN: Id of an existing HDF5 object
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
H5Object::H5Object( const hid_t object_id ) : IdComponent( object_id ) {}

#endif // DOXYGEN_SHOULD_SKIP_THIS

//--------------------------------------------------------------------------
// Function:	H5Object copy constructor
///\brief	Copy constructor: makes a copy of the original H5Object
///		instance.
///\param	original - IN: H5Object instance to copy
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
H5Object::H5Object( const H5Object& original ) : IdComponent( original ) {}

//--------------------------------------------------------------------------
// Function:	H5Object::createAttribute
///\brief	Creates an attribute for a group, dataset, or named datatype.
///\param	name - IN: Name of the attribute
///\param	data_type - IN: Datatype for the attribute
///\param	data_space - IN: Dataspace for the attribute - only simple
///		dataspaces are allowed at this time
///\param	create_plist - IN: Creation property list - default to
///		PropList::DEFAULT
///\return	Attribute instance
///\exception	H5::AttributeIException
///\par Description
///		The attribute name specified in \a name must be unique.
///		Attempting to create an attribute with the same name as an
///		existing attribute will raise an exception, leaving the
///		pre-existing attribute intact. To overwrite an existing
///		attribute with a new attribute of the same name, first
///		delete the existing one with \c H5Object::removeAttr, then
///		recreate it with this function.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
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
      throw AttributeIException(inMemFunc("createAttribute"), "H5Acreate failed");
   }
}

//--------------------------------------------------------------------------
// Function:	H5Object::createAttribute
///\brief	This is an overloaded member function, provided for convenience.
///		It differs from the above function in that it takes
///		a reference to an \c std::string for \a name.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
Attribute H5Object::createAttribute( const H5std_string& name, const DataType& data_type, const DataSpace& data_space, const PropList& create_plist ) const
{
   return( createAttribute( name.c_str(), data_type, data_space, create_plist ));
}

//--------------------------------------------------------------------------
// Function:	H5Object::openAttribute
///\brief	Opens an attribute given its name.
///\param	name - IN: Name of the attribute
///\return	Attribute instance
///\exception	H5::AttributeIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
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
      throw AttributeIException(inMemFunc("openAttribute"), "H5Aopen_name failed");
   }
}

//--------------------------------------------------------------------------
// Function:	H5Object::openAttribute
///\brief	This is an overloaded member function, provided for convenience.
///		It differs from the above function in that it takes
///		a reference to an \c std::string for \a name.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
Attribute H5Object::openAttribute( const H5std_string& name ) const
{
   return( openAttribute( name.c_str()) );
}

//--------------------------------------------------------------------------
// Function:	H5Object::openAttribute
///\brief	Opens an attribute given its index.
///\param	idx - IN: Index of the attribute, a 0-based, non-negative integer
///\return	Attribute instance
///\exception	H5::AttributeIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
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
      throw AttributeIException(inMemFunc("openAttribute"), "H5Aopen_idx failed");
   }
}

//--------------------------------------------------------------------------
// Function:	H5Object::iterateAttrs
///\brief	Iterates a user's function over all the attributes of an H5
///		object, which may be a group, dataset or named datatype.
///\param	user_op - IN: User's function to operate on each attribute
///\param	idx - IN/OUT: Starting (IN) and ending (OUT) attribute indices
///\param	op_data - IN: User's data to pass to user's operator function
///\return	Returned value of the last operator if it was non-zero, or
///		zero if all attributes were processed
///\exception	H5::AttributeIException
///\par Description
///		For information, please refer to the C layer Reference Manual
///		at:
/// http://hdf.ncsa.uiuc.edu/HDF5/doc/RM_H5A.html#Annot-Iterate
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
int H5Object::iterateAttrs( attr_operator_t user_op, unsigned *_idx, void *op_data )
{
   // store the user's function and data
   UserData4Aiterate* userData = new UserData4Aiterate;
   userData->opData = op_data;
   userData->op = user_op;
   userData->object = this;

   // call the C library routine H5Aiterate2 to iterate the attributes
   hsize_t idx = (hsize_t)*_idx;
   int ret_value = H5Aiterate2(id, ".", H5_INDEX_NAME, H5_ITER_INC, &idx, userAttrOpWrpr, (void *) userData, H5P_DEFAULT);

   // release memory
   delete userData;

   if( ret_value >= 0 ) {
      /* Pass back update index value to calling code */
      *_idx = (unsigned)idx;

      return( ret_value );
   }
   else  // raise exception when H5Aiterate returns a negative value
      throw AttributeIException(inMemFunc("iterateAttrs"), "H5Aiterate2 failed");
}

//--------------------------------------------------------------------------
// Function:	H5Object::getNumAttrs
///\brief	Returns the number of attributes attached to this HDF5 object.
///\return	Number of attributes
///\exception	H5::AttributeIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
int H5Object::getNumAttrs() const
{
   int num_attrs = H5Aget_num_attrs( id );
   if( num_attrs < 0 )
   {
      throw AttributeIException(inMemFunc("getNumAttrs"),
		"H5Aget_num_attrs failed - returned negative number of attributes");
   }
   else
      return( num_attrs );
}

//--------------------------------------------------------------------------
// Function:	H5Object::removeAttr
///\brief	Removes the named attribute from this object.
///\param	name - IN: Name of the attribute to be removed
///\exception	H5::AttributeIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void H5Object::removeAttr( const char* name ) const
{
   herr_t ret_value = H5Adelete2(id, ".", name, H5P_DEFAULT);
   if( ret_value < 0 )
      throw AttributeIException(inMemFunc("removeAttr"), "H5Adelete2 failed");
}

//--------------------------------------------------------------------------
// Function:	H5Object::removeAttr
///\brief	This is an overloaded member function, provided for convenience.
///		It differs from the above function in that it takes
///		a reference to an \c std::string for \a name.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void H5Object::removeAttr( const H5std_string& name ) const
{
   removeAttr( name.c_str() );
}

//--------------------------------------------------------------------------
// Function:	H5Object::renameAttr
///\brief	Renames the named attribute from this object.
///\param	oldname - IN: Name of the attribute to be renamed
///\param	newname - IN: New name ame of the attribute
///\exception	H5::AttributeIException
// Programmer	Binh-Minh Ribler - Mar, 2005
//--------------------------------------------------------------------------
void H5Object::renameAttr(const char* oldname, const char* newname) const
{
   herr_t ret_value = H5Arename2(id, ".", oldname, newname, H5P_DEFAULT);
   if (ret_value < 0)
      throw AttributeIException(inMemFunc("renameAttr"), "H5Arename2 failed");
}

//--------------------------------------------------------------------------
// Function:	H5Object::renameAttr
///\brief	This is an overloaded member function, provided for convenience.
///		It differs from the above function in that it takes
///		a reference to an \c std::string for the names.
// Programmer	Binh-Minh Ribler - Mar, 2005
//--------------------------------------------------------------------------
void H5Object::renameAttr(const H5std_string& oldname, const H5std_string& newname) const
{
   renameAttr (oldname.c_str(), newname.c_str());
}

//--------------------------------------------------------------------------
// Function:	H5Object::flush
///\brief	Flushes all buffers associated with a file to disk.
///\param	scope - IN: Specifies the scope of the flushing action,
///		which can be either of these values:
///		\li \c H5F_SCOPE_GLOBAL - Flushes the entire virtual file
///		\li \c H5F_SCOPE_LOCAL - Flushes only the specified file
///\exception	H5::AttributeIException
///\par Description
///		This object is used to identify the file to be flushed.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void H5Object::flush(H5F_scope_t scope ) const
{
   herr_t ret_value = H5Fflush( id, scope );
   if( ret_value < 0 )
   {
      throw FileIException(inMemFunc("flush"), "H5Fflush failed");
   }
}

//--------------------------------------------------------------------------
// Function:	H5Object::getFileName
///\brief	Gets the name of the file, in which this HDF5 object belongs.
///\return	File name
///\exception	H5::IdComponentException
// Programmer	Binh-Minh Ribler - Jul, 2004
//--------------------------------------------------------------------------
H5std_string H5Object::getFileName() const
{
   try {
      return(p_get_file_name());
   }
   catch (IdComponentException E) {
      throw FileIException(inMemFunc("getFileName"), E.getDetailMsg());
   }
}

//--------------------------------------------------------------------------
// Function:	H5Object destructor
///\brief	Noop destructor.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
H5Object::~H5Object() {}

#ifndef H5_NO_NAMESPACE
} // end namespace
#endif
