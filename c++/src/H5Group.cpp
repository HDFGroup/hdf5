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
#ifdef OLD_HEADER_FILENAME
#include <iostream.h>
#else
#include <iostream>
#endif

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

//--------------------------------------------------------------------------
// Function:	Group default constructor
///\brief	Default constructor: Creates a stub group
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
///\exception	H5::GroupIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
Group::Group( const hid_t group_id ) : H5Object( group_id ) {}

//--------------------------------------------------------------------------
// Function:	Group::getNumObjs
///\brief	Returns the number of objects in this group.
///\exception	H5::GroupIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
hsize_t Group::getNumObjs() const
{
   hsize_t num_objs;
   herr_t ret_value = H5Gget_num_objs(id, &num_objs);
   if(ret_value < 0)
   {
      throwException("getNumObjs", "H5Gget_num_objs failed");
   }
   return (num_objs);
}

//--------------------------------------------------------------------------
// Function:	Group::getObjnameByIdx
///\brief	Retrieves the name of an object in this group by giving the
///		object's index.
///\return	Object name
///\exception	H5::GroupIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
ssize_t Group::getObjnameByIdx(hsize_t idx, string& name, size_t size) const
{
   char* name_C = new char[size];
   ssize_t name_len = H5Gget_objname_by_idx(id, idx, name_C, size);
   if(name_len < 0)
   {
      throwException("getObjnameByIdx", "H5Gget_objname_by_idx failed");
   }
   name = string( name_C );
   delete [] name_C;
   return (name_len);
}

//--------------------------------------------------------------------------
// Function:	Group::getObjTypeByIdx
///\brief	Returns the type of an object in this group by giving the
///		object's index.
///\return	Object type
///\exception	H5::GroupIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
int Group::getObjTypeByIdx(hsize_t idx) const
{
   int obj_type = H5Gget_objtype_by_idx(id, idx);
   if (obj_type == H5G_UNKNOWN)
   {
	   throwException("getObjTypeByIdx", "H5Gget_objtype_by_idx failed");
   }
   return (obj_type);
}

//--------------------------------------------------------------------------
// Function:	Group::getObjTypeByIdx
///\brief	This is an overloaded member function, provided for convenience.
///		It differs from the above function because it also provides 
///		the returned object type in text.
///\param	idx - IN: Index of the object
///\param	type_name - IN: Object type in text
///\return	Object type
///\exception	H5::GroupIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
int Group::getObjTypeByIdx(hsize_t idx, string& type_name) const
{
   int obj_type = H5Gget_objtype_by_idx(id, idx);
   switch (obj_type)
   {
	case H5G_GROUP: type_name = string("group"); break;
	case H5G_DATASET: type_name = string("dataset"); break;
	case H5G_TYPE: type_name = string("datatype"); break;
	case H5G_UNKNOWN:
	default:
   	{
	   throwException("getObjTypeByIdx", "H5Gget_objtype_by_idx failed");
	}
   }
   return (obj_type);
}

// Iterates a user's function over the entries of a group.
//int Group::iterateElems( const string& name, int *idx, H5G_iterate_t op , void *op_data )
//{
   //return( iterateElems( name.c_str(), idx, op, op_data ));
//}
//int Group::iterateElems( const char* name, int *idx, H5G_iterate_t op , void *op_data )
//{
   //int ret_value = H5Giterate( id, name, idx, op, op_data );
   //if( ret_value >= 0 )
      //return( ret_value );
   //else  // raise exception when H5Aiterate returns a negative value
   //{
      //throw GroupIException("Group::iterateElems", "H5Giterate failed");
   //}
//}

//--------------------------------------------------------------------------
// Function:	Group::p_close (private)
///\brief	Closes this group.
///\exception   H5::GroupIException
///\note
///		This function will be obsolete because its functionality 
///		is recently handled by the C library layer.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void Group::p_close() const
{
   herr_t ret_value = H5Gclose( id );
   if( ret_value < 0 )
   {
      throw GroupIException(0, "H5Gclose failed");
   }
}

//--------------------------------------------------------------------------
// Function:	Group::throwException
///\brief	Throws group exception - initially implemented for CommonFG
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
//--------------------------------------------------------------------------
Group::~Group()
{  
   // The group id will be closed properly
    try {
        resetIdComponent( this ); }
    catch (Exception close_error) { // thrown by p_close
        cerr << "Group::~Group - " << close_error.getDetailMsg() << endl;
    }
}  

#ifndef H5_NO_NAMESPACE
} // end namespace
#endif
