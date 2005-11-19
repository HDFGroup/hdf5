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

#ifdef OLD_HEADER_FILENAME
#include <iostream.h>
#else
#include <iostream>
#endif
#include <string>

#ifndef H5_NO_NAMESPACE
#ifndef H5_NO_STD
    using std::string;
#endif  // H5_NO_STD
#endif

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
//--------------------------------------------------------------------------
Group::Group() : H5Object() {}

//--------------------------------------------------------------------------
// Function:	Group copy constructor
///\brief	Copy constructor: makes a copy of the original Group object.
///\param	original - IN: Original group to copy
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
Group::Group( const Group& original ) : H5Object( original ) {}

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
//--------------------------------------------------------------------------
Group::Group( const hid_t group_id ) : H5Object( group_id ) {}

//--------------------------------------------------------------------------
// Function:	Group::Reference
///\brief	Creates a reference to an HDF5 object or a dataset region.
///\param	name - IN: Name of the object to be referenced
///\param	dataspace - IN: Dataspace with selection
///\param	ref_type - IN: Type of reference; default to \c H5R_DATASET_REGION
///\return	A reference
///\exception	H5::IdComponentException
// Programmer	Binh-Minh Ribler - May, 2004
//--------------------------------------------------------------------------
void* Group::Reference(const char* name, DataSpace& dataspace, H5R_type_t ref_type) const
{
   try {
      return(p_reference(name, dataspace.getId(), ref_type));
   }
   catch (IdComponentException E) {
      throw GroupIException("Group::Reference", E.getDetailMsg());
   }
}

//--------------------------------------------------------------------------
// Function:	Group::Reference
///\brief	This is an overloaded function, provided for your convenience.
///		It differs from the above function in that it only creates
///		a reference to an HDF5 object, not to a dataset region.
///\param	name - IN: Name of the object to be referenced
///\return	A reference
///\exception	H5::IdComponentException
///\par Description
//		This function passes H5R_OBJECT and -1 to the protected
//		function for it to pass to the C API H5Rcreate
//		to create a reference to the named object.
// Programmer	Binh-Minh Ribler - May, 2004
//--------------------------------------------------------------------------
void* Group::Reference(const char* name) const
{
   try {
      return(p_reference(name, -1, H5R_OBJECT));
   }
   catch (IdComponentException E) {
      throw GroupIException("Group::Reference", E.getDetailMsg());
   }
}

//--------------------------------------------------------------------------
// Function:	Group::Reference
///\brief	This is an overloaded function, provided for your convenience.
///		It differs from the above function in that it takes an
///		\c std::string for the object's name.
///\param	name - IN: Name of the object to be referenced
// Programmer	Binh-Minh Ribler - May, 2004
//--------------------------------------------------------------------------
void* Group::Reference(const string& name) const
{
   return(Reference(name.c_str()));
}

//--------------------------------------------------------------------------
// Function:	Group::getObjType
///\brief	Retrieves the type of object that an object reference points to.
///\param		ref      - IN: Reference to query
///\param		ref_type - IN: Type of reference to query
// Return	An object type, which can be one of the following:
//			H5G_LINK Object is a symbolic link.
//			H5G_GROUP Object is a group.
//			H5G_DATASET   Object is a dataset.
//			H5G_TYPE Object is a named datatype
// Exception	H5::IdComponentException
// Programmer	Binh-Minh Ribler - May, 2004
//--------------------------------------------------------------------------
H5G_obj_t Group::getObjType(void *ref, H5R_type_t ref_type) const
{
   return(p_get_obj_type(ref, ref_type));
}

//--------------------------------------------------------------------------
// Function:	Group::getRegion
///\brief	Retrieves a dataspace with the region pointed to selected.
///\param	ref      - IN: Reference to get region of
///\param	ref_type - IN: Type of reference to get region of - default
///\return	DataSpace instance
///\exception	H5::IdComponentException
// Programmer	Binh-Minh Ribler - May, 2004
//--------------------------------------------------------------------------
DataSpace Group::getRegion(void *ref, H5R_type_t ref_type) const
{
   DataSpace dataspace(p_get_region(ref, ref_type));
   return(dataspace);
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
   herr_t ret_value = H5Gclose( id );
   if( ret_value < 0 )
   {
      throw GroupIException("Group::close", "H5Gclose failed");
   }
   // reset the id because the group that it represents is now closed
   id = 0;
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
void Group::throwException(const string func_name, const string msg) const
{
   string full_name = func_name;
   full_name.insert(0, "Group::");
   throw GroupIException(full_name, msg);
}

//--------------------------------------------------------------------------
// Function:	Group destructor
///\brief	Properly terminates access to this group.
// Programmer	Binh-Minh Ribler - 2000
// Modification
//		Replaced resetIdComponent with decRefCount to use C library
//		ID reference counting mechanism - BMR, Feb 20, 2005
//--------------------------------------------------------------------------
Group::~Group()
{
   // The group id will be closed properly
    try {
        decRefCount();
    }
    catch (Exception close_error) {
        cerr << "Group::~Group - " << close_error.getDetailMsg() << endl;
    }
}

#ifndef H5_NO_NAMESPACE
} // end namespace
#endif
