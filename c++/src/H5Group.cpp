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

// Get id of the location, which id the group id here; used by CommonFG
// member functions
hid_t Group::getLocId() const
{
   return( getId() );
}

// Creates a copy of an existing Group using its id
Group::Group( const hid_t group_id ) : H5Object( group_id ) {}

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

// Calls the C API H5Gclose to close this group.  Used by IdComponent::reset
void Group::p_close() const
{
   herr_t ret_value = H5Gclose( id );
   if( ret_value < 0 )
   {
      throw GroupIException(NULL, "H5Gclose failed");
   }
}

// Throw file exception
void Group::throwException(const string& func_name, const string& msg) const
{
   string full_name = func_name;
   full_name.insert(0, "Group::");
   throw GroupIException(full_name, msg);
}

// The destructor of this instance calls IdComponent::reset to
// reset its identifier - no longer true
// Older compilers (baldric) don't support template member functions
// and IdComponent::reset is one; so at this time, the resetId is not
// a member function so it can be template to work around that problem.
Group::~Group()
{  
   // The group id will be closed properly
    try {
        resetIdComponent( this ); }
    catch (Exception close_error) { // thrown by p_close
        throw GroupIException("Group::~Group", close_error.getDetailMsg());
    }

}  

#ifndef H5_NO_NAMESPACE
} // end namespace
#endif
