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

#ifdef OLD_HEADER_FILENAME
#include <iostream.h>
#else
#include <iostream>
#endif
#include <string>

#include "H5Include.h"
#include "H5Exception.h"
#include "H5IdComponent.h"
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
#ifndef H5_NO_STD
    using std::cerr;
    using std::endl;
#endif  // H5_NO_STD
#endif

//--------------------------------------------------------------------------
// Function:	Group default constructor
///\brief	Default constructor: creates a stub Group.
// Programmer	Binh-Minh Ribler - 2000
// Modification
//	2008 08	No longer inherit data member 'id' from IdComponent.
//		- bugzilla 1068
//--------------------------------------------------------------------------
Group::Group() : H5Object(), id(0) {}

//--------------------------------------------------------------------------
// Function:	Group copy constructor
///\brief	Copy constructor: makes a copy of the original Group object.
///\param	original - IN: Original group to copy
// Programmer	Binh-Minh Ribler - 2000
// Modification
//	Jul, 08	No longer inherit data member 'id' from IdComponent.
//		- bugzilla 1068
//--------------------------------------------------------------------------
Group::Group(const Group& original) : H5Object(original)
{
    id = original.getId();
    incRefCount(); // increment number of references to this id
}

//--------------------------------------------------------------------------
// Function:	Group::getLocId
///\brief	Returns the id of this group.
///\return	Id of this group
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
hid_t Group::getLocId() const
{
   return( getId() );
}

//--------------------------------------------------------------------------
// Function:	Group overloaded constructor
///\brief	Creates a Group object using the id of an existing group.
///\param	group_id - IN: Id of an existing group
// Programmer	Binh-Minh Ribler - 2000
// Modification
//	Jul, 08	No longer inherit data member 'id' from IdComponent.
//		- bugzilla 1068
//--------------------------------------------------------------------------
Group::Group(const hid_t existing_id) : H5Object()
{
    id = existing_id;
}

//--------------------------------------------------------------------------
// Function:	Group overload constructor - dereference
///\brief	Given a reference to some object or a file, returns that group
///\param	obj - IN: Location reference object is in
///		ref - IN: Reference pointer
///\par Description
///		\c obj can be DataSet, Group, H5File, or named DataType, that 
///		is a datatype that has been named by DataType::commit.
// Programmer	Binh-Minh Ribler - Oct, 2006
//--------------------------------------------------------------------------
Group::Group(H5Object& obj, void* ref, H5R_type_t ref_type) : H5Object()
{
   dereference(obj, ref, ref_type);
}

Group::Group(H5File& h5file, void* ref, H5R_type_t ref_type) : H5Object()
{
   dereference(h5file, ref, ref_type);
}

#if 0 // remove after verifying with tests
//--------------------------------------------------------------------------
// Function:	Group::getObjType
///\brief	Retrieves the type of object that an object reference points to.
///\param	ref      - IN: Reference to query
///		ref_type - IN: Type of reference to query, valid values are:
///		\li \c H5R_OBJECT \tReference is an object reference.
///		\li \c H5R_DATASET_REGION \tReference is a dataset region reference.
///\return	An object type, which can be one of the following:
///			H5G_LINK Object is a symbolic link.
///			H5G_GROUP Object is a group.
///			H5G_DATASET   Object is a dataset.
///			H5G_TYPE Object is a named datatype
///\exception	H5::GroupIException
// Programmer	Binh-Minh Ribler - May, 2004
//--------------------------------------------------------------------------
H5G_obj_t Group::getObjType(void *ref, H5R_type_t ref_type) const
{
   try {
      return(p_get_obj_type(ref, ref_type));
   }
   catch (IdComponentException E) {
      throw GroupIException("Group::getObjType", E.getDetailMsg());
   }
}
#endif

//--------------------------------------------------------------------------
// Function:	H5File::getObjType
///\brief	This function was misnamed and will be deprecated in favor of
///		H5Object::getRefObjType; please use getRefObjType instead.
// Programmer	Binh-Minh Ribler - May, 2004
// Note:	Replaced by getRefObjType. - BMR - Jul, 2008
//--------------------------------------------------------------------------
H5G_obj_t Group::getObjType(void *ref, H5R_type_t ref_type) const
{
    return(getRefObjType(ref, ref_type));
}

//--------------------------------------------------------------------------
// Function:	Group::getRegion
///\brief	Retrieves a dataspace with the region pointed to selected.
///\param	ref      - IN: Reference to get region of
///		ref_type - IN: Type of reference to get region of - default
///\return	DataSpace instance
///\exception	H5::GroupIException
// Programmer	Binh-Minh Ribler - May, 2004
//--------------------------------------------------------------------------
DataSpace Group::getRegion(void *ref, H5R_type_t ref_type) const
{
   try {
      DataSpace dataspace(p_get_region(ref, ref_type));
      return(dataspace);
   }
   catch (IdComponentException E) {
      throw GroupIException("Group::getRegion", E.getDetailMsg());
   }
}

//--------------------------------------------------------------------------
// Function:	Group::getId
// Purpose:	Get the id of this group
// Modification:
//	May 2008 - BMR
//		Class hierarchy is revised to address bugzilla 1068.  Class
//		AbstractDS and Attribute are moved out of H5Object.  In
//		addition, member IdComponent::id is moved into subclasses, and
//		IdComponent::getId now becomes pure virtual function.
// Programmer	Binh-Minh Ribler - May, 2008
//--------------------------------------------------------------------------
hid_t Group::getId() const
{
   return(id);
}

//--------------------------------------------------------------------------
// Function:	Group::p_setId
///\brief	Sets the identifier of this group to a new value.
///
///\exception	H5::IdComponentException when the attempt to close the 
///		currently open group fails
// Description:
//		The underlaying reference counting in the C library ensures
//		that the current valid id of this object is properly closed.
//		Then the object's id is reset to the new id.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void Group::p_setId(const hid_t new_id)
{
    // handling references to this old id
    try {
	close();
    }
    catch (Exception close_error) {
	throw GroupIException("Group::p_setId", close_error.getDetailMsg());
    }
    // reset object's id to the given id
    id = new_id;
}

//--------------------------------------------------------------------------
// Function:	Group::close
///\brief	Closes this group.
///
///\exception	H5::GroupIException
// Programmer	Binh-Minh Ribler - Mar 9, 2005
//--------------------------------------------------------------------------
void Group::close()
{
    if (p_valid_id(id))
    {
	herr_t ret_value = H5Gclose( id );
	if( ret_value < 0 )
	{
	    throw GroupIException("Group::close", "H5Gclose failed");
	}
	// reset the id because the group that it represents is now closed
	id = 0;
    }
}

//--------------------------------------------------------------------------
// Function:	Group::throwException
///\brief	Throws H5::GroupIException.
///\param	func_name - Name of the function where failure occurs
///\param	msg       - Message describing the failure
///\exception	H5::GroupIException
// Description
//		This function is used in CommonFG implementation so that
//		proper exception can be thrown for file or group.  The
//		argument func_name is a member of CommonFG and "Group::"
//		will be inserted to indicate the function called is an
//		implementation of Group.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void Group::throwException(const H5std_string& func_name, const H5std_string& msg) const
{
   H5std_string full_name = func_name;
   full_name.insert(0, "Group::");
   throw GroupIException(full_name, msg);
}

//--------------------------------------------------------------------------
// Function:	Group destructor
///\brief	Properly terminates access to this group.
// Programmer	Binh-Minh Ribler - 2000
// Modification
//		- Replaced resetIdComponent() with decRefCount() to use C
//		library ID reference counting mechanism - BMR, Feb 20, 2005
//		- Replaced decRefCount with close() to let the C library
//		handle the reference counting - BMR, Jun 1, 2006
//--------------------------------------------------------------------------
Group::~Group()
{
    try {
	close();
    }
    catch (Exception close_error) {
	cerr << "Group::~Group - " << close_error.getDetailMsg() << endl;
    }
}

#ifndef H5_NO_NAMESPACE
} // end namespace
#endif
