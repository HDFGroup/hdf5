#include <string>

#include "H5Include.h"
#include "H5RefCounter.h"
#include "H5Exception.h"
#include "H5IdComponent.h"
#include "H5Idtemplates.h"
#include "H5PropList.h"

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

const PropList PropList::DEFAULT( H5P_DEFAULT );

// Default constructor - set id to 0 by default here but may be set
// to a valid one, if any, by a subclass constructor.
PropList::PropList() : IdComponent( 0 ) {}

// Creates a new property of specified type
PropList::PropList( H5P_class_t type ) : IdComponent( 0 )
{
   // call C routine to create the new property
   id = H5Pcreate(type );
   if( id <= 0 )
   {
      throw PropListIException();
   }
}

// Copy constructor: makes a copy of the original object
PropList::PropList( const PropList& original ) : IdComponent( original ) {}

/* Constructor that takes an existing property list id.
Description:
	Uses an HDF5 id to create a PropList identifier object.  This id
	can be either an existing property list id or a default property
	list id.  Design note: in the case of default property list,
	the identifier still has reference counter; the p_close function
	will take care of not to call H5Pclose on the default id.
*/
PropList::PropList( const hid_t plist_id ) : IdComponent( plist_id ) { }

// Makes a copy of an existing property list 
void PropList::copy( const PropList& like_plist )
{
   // reset the identifier of this PropList - send 'this' in so that
   // H5Pclose can be called appropriately
   resetIdComponent( this );

   // call C routine to copy the property list
   id = H5Pcopy( like_plist.getId() );

   // points to the same ref counter
   ref_count = new RefCounter;

   if( id <= 0 )
   {
      throw PropListIException();
   }
}

// Closes the property list if it is not a default one
void PropList::p_close() const
{
   if( id != H5P_DEFAULT ) // not a constant, should call H5Pclose
   {
      herr_t ret_value = H5Pclose( id );
      if( ret_value < 0 )
      {
         throw PropListIException("PropList::p_close: unable to close the property list.  Please report this bug to HDF." );
      }
   }
}

// Returns the class of this property list, i.e. H5P_FILE_CREATE...
H5P_class_t PropList::getClass() const
{
   H5P_class_t plist_class = H5Pget_class( id );
   if( plist_class == H5P_NO_CLASS )
   {
      throw PropListIException();
   }
   return( plist_class );
}

// The destructor of this instance calls the template resetIdComponent to
// reset its identifier
PropList::~PropList()
{  
   // The property list id will be closed properly
   resetIdComponent( this );
}  

#ifndef H5_NO_NAMESPACE
} // end namespace
#endif
