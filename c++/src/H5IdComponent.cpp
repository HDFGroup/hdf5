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
IdComponent::IdComponent() : id( 0 )
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
   resetIdComponent( this );

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
   resetIdComponent( this );

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

#ifndef H5_NO_NAMESPACE
}
#endif
