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
#include "H5Library.h"
#include "H5IdComponent.h"
#include "H5Idtemplates.h"

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

// Default constructor - private
IdComponent::IdComponent() : id( -1 )
{
   // starts counting object references
   ref_count = new RefCounter;
}

// Constructor that takes an HDF5 object id.  It creates an instance
// of IdComponent to hold the HDF5 id
IdComponent::IdComponent( const hid_t h5_id ) : id( h5_id )
{
   // starts counting object references
   ref_count = new RefCounter;
}

// Copy constructor: makes a copy of the original object
IdComponent::IdComponent( const IdComponent& original )
{
   id = original.id;
   ref_count = original.ref_count; // points to the same ref counter
   ref_count->increment(); // increment number of references to this id
}

// Increment reference counter
void IdComponent::incRefCount() { ref_count->increment(); }

// Decrement reference counter
void IdComponent::decRefCount() { ref_count->decrement(); }

// Get the reference counter to this identifier
int IdComponent::getCounter() { return( ref_count->getCounter()); }

// Decrements the reference counter then determines if there are no more
// reference to this object
bool IdComponent::noReference()
{
   if( ref_count->getCounter() > 0 )
      ref_count->decrement();
   return( ref_count->getCounter() == 0 ? true:false );
}

/* Assignment operator.
   Description:
   Reset the identifier of this object so that the HDF5 id can be properly
   closed.  Copy the new identifier to this object, then increment the 
   reference counter of the identifier to indicate that another object 
   is referencing the identifier.
*/
IdComponent& IdComponent::operator=( const IdComponent& rhs )
{
   // reset the identifier of this object - resetIdComponent will call the 
   // appropriate H5xclose to close the id
    try {
        resetIdComponent( this ); }
    catch (Exception close_error) { // thrown by p_close
        throw IdComponentException("IdComponent::operator=", close_error.getDetailMsg());
    }

   // copy the data members from the rhs object
   id = rhs.id;
   ref_count = rhs.ref_count; // points to the same ref counter

   // increment the reference counter
   ref_count->increment();

   return( *this );
}

/* Sets the identifier of this object to a new value
   Description:
        Reset the current identifier of this object so that the HDF5
        id can be appropriately closed.  If only this object references
        its identifier, its reference counter will be deleted.  A new 
        reference counter is created for the new HDF5 object id.
*/
void IdComponent::setId( hid_t new_id )
{
   // reset the identifier of this object, call appropriate H5Xclose
    try {
        resetIdComponent( this ); }
    catch (Exception close_error) { // thrown by p_close
        throw IdComponentException("IdComponent::setId", close_error.getDetailMsg());
    }

   id = new_id;

   // starts counting object references
   ref_count = new RefCounter;
}

// Gets the id of this object 
hid_t IdComponent::getId () const
{
   return( id );
}

// Reset this object by deleting its RefCounter
void IdComponent::reset ()
{
   delete ref_count;
   ref_count = NULL;
}

// Default destructor
IdComponent::~IdComponent() {

/* uncomment this block and complete it when deciding to use dontAtExit 
   unless the atexit/global destructor problem is fixed, then 
   remove it- BMR 11/14/00

   if( id == NOTATEXIT )
   {
      // Call H5Library::close to clean up - temporary solution to avoid the
      // trouble of atexit/global destructors
      try {
         if( H5Library::need_cleanup == true )
         {
            H5Library::close();
            H5Library::need_cleanup = false; // reset the boolean just in case
         }
      }
      // catch failure caused by the H5Library operations
      catch( LibraryIException error )
      {
         error.printError();
      }
   }
*/
}

//
// Implementation for HDF5 Reference Interface 
// 

//--------------------------------------------------------------------------
// Function:	IdComponent::p_reference (protected)
// Purpose	Creates a reference to an HDF5 object or a dataset region.
// Parameters
//		name - IN: Name of the object to be referenced
//		dataspace - IN: Dataspace with selection
//		ref_type - IN: Type of reference; default to \c H5R_DATASET_REGION
// Return	A reference
// Exception	H5::IdComponentException
// Programmer	Binh-Minh Ribler - May, 2004
//--------------------------------------------------------------------------
void* IdComponent::p_reference(const char* name, hid_t space_id, H5R_type_t ref_type) const
{
   void *ref;
   herr_t ret_value = H5Rcreate(ref, id, name, ref_type, space_id);
   if (ret_value < 0)
   {
      throw IdComponentException("IdComponent::p_reference",
                "H5Rcreate failed");
   }
   return(ref);
}

//--------------------------------------------------------------------------
// Function:	IdComponent::p_get_obj_type (protected)
// Purpose	Retrieves the type of object that an object reference points to.
// Parameters
//		ref      - IN: Reference to query
//		ref_type - IN: Type of reference to query
// Return	An object type, which can be one of the following:
//			H5G_LINK Object is a symbolic link.  
//			H5G_GROUP Object is a group.  
//			H5G_DATASET   Object is a dataset.  
//			H5G_TYPE Object is a named datatype 
// Exception	H5::IdComponentException
// Programmer	Binh-Minh Ribler - May, 2004
//--------------------------------------------------------------------------
H5G_obj_t IdComponent::p_get_obj_type(void *ref, H5R_type_t ref_type) const
{
   H5G_obj_t obj_type = H5Rget_obj_type(id, ref_type, ref);
   if (obj_type == H5G_UNKNOWN)
   {
      throw IdComponentException("IdComponent::p_get_obj_type",
                "H5R_get_obj_type failed");
   }
   return(obj_type);
}

//--------------------------------------------------------------------------
// Function:	IdComponent::p_get_region (protected)
// Purpose	Retrieves a dataspace with the region pointed to selected.
// Parameters
//		ref_type - IN: Type of reference to get region of - default
//				to H5R_DATASET_REGION
//		ref      - IN: Reference to get region of
// Return	Dataspace id
// Exception	H5::IdComponentException
// Programmer	Binh-Minh Ribler - May, 2004
//--------------------------------------------------------------------------
hid_t IdComponent::p_get_region(void *ref, H5R_type_t ref_type) const
{
   hid_t space_id = H5Rget_region(id, ref_type, ref);
   if (space_id < 0)
   {
      throw IdComponentException("IdComponent::p_get_region",
                "H5Rget_region failed");
   }
   return(space_id);
}

#ifndef H5_NO_NAMESPACE
}
#endif
