#ifndef _Reset_h
#define _Reset_h

#include "H5IdComponent.h"

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

// Older compilers (baldric) don't support template member functions
// and IdComponent::reset is one; so at this time, the resetId is not
// a member function so it can be template to work around that problem.

template <class Type>
void resetIdComponent( 
	Type* obj )	// pointer to object to be reset
{
   if( obj->noReference())  // ref count of this object is decremented here
   {
      if( obj->getId() > 0 )
      {
         obj->p_close();  // which p_close depends on whom this
                             // IdComponent object belongs to
      }
      obj->reset();  // delete ref_count from IdComponent
   }
}

#ifndef H5_NO_NAMESPACE
}
#endif
#endif
