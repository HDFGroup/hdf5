#ifndef _H5RefCounter_H
#define _H5RefCounter_H

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

// define bool type for platforms that don't support bool yet
// Note: it is added here because most of the C++ library source
// files include this header file
#ifdef BOOL_NOTDEFINED
#ifdef false
#undef false
#endif
#ifdef true
#undef true
#endif
typedef int bool;
const bool  false = 0;
const bool  true  = 1;

#endif

class RefCounter {
   public:
	// Creates a reference counter to be used by an HDF5 object
	RefCounter();

        int getCounter () const;
        void increment();
        void decrement();

	// this bool function is used to determine whether to close an
	// HDF5 object when there are no more reference to that object
	bool noReference();

	~RefCounter();

   private:
	int counter; // keeps track of number of copies of an object
};
#ifndef H5_NO_NAMESPACE
}
#endif
#endif
