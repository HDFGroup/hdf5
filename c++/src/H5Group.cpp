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
      //throw GroupIException();
   //}
//}

// Calls the C API H5Gclose to close this group.  Used by IdComponent::reset
void Group::p_close() const
{
   herr_t ret_value = H5Gclose( id );
   if( ret_value < 0 )
   {
      throw GroupIException();
   }
}

// Throw file exception
void Group::throwException() const
{
   throw GroupIException();
}

// The destructor of this instance calls IdComponent::reset to
// reset its identifier - no longer true
// Older compilers (baldric) don't support template member functions
// and IdComponent::reset is one; so at this time, the resetId is not
// a member function so it can be template to work around that problem.
Group::~Group()
{  
   // The group id will be closed properly
   resetIdComponent( this );
}  

#ifndef H5_NO_NAMESPACE
} // end namespace
#endif
