#include "H5RefCounter.h"

#ifndef H5_NO_NAMESPACE
namespace H5 {
using namespace std;
#endif

// Creates a reference counter to be used by an HDF5 object
RefCounter::RefCounter() : counter(1) {} 
 
// Returns the current value of the reference counter
int RefCounter::getCounter () const { return counter; }

// Increments the reference counter as a copy of the object that uses
// this counter is created.
void RefCounter::increment() { counter++; }

// Decrements the reference counter as a copy of the object that uses
// this counter is destroyed.
void RefCounter::decrement() { counter--; }

// Decrements the reference counter then determines if there are no more 
// reference to the object that uses this counter
bool RefCounter::noReference()
{
   if( counter > 0 )
      counter--;
   return( counter == 0 ? true:false );
}

RefCounter::~RefCounter() {}

#ifndef H5_NO_NAMESPACE
} // end namespace
#endif
