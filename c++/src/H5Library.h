#ifndef _H5Library_H
#define _H5Library_H

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

#define NOTATEXIT       (-10)   // just in case the HDF5 library use more
	// negative constants. Note: the solution used for the atexit/global
	// destructors is not reliable, and desperately needs improvement 
	// It is not even working, inifiteloop message still printed when
	// calling H5close

class H5Library {
   public:
	static bool need_cleanup; // indicates if H5close should be called

	// Initializes the HDF5 library. 
	static void open(); 

	// Flushes all data to disk, closes files, and cleans up memory. 
	static void close(); 

	// Instructs library not to install atexit cleanup routine
	static void dontAtExit(); 

	// Returns the HDF library release number. 
	static void getLibVersion( unsigned& majnum, unsigned& minnum, unsigned& relnum ); 

	// Verifies that the arguments match the version numbers compiled
	// into the library
	static void checkVersion( unsigned majnum, unsigned minnum, unsigned relnum ); 

   private:
	// Default constructor - no instance ever created
	H5Library() {};

};
#ifndef H5_NO_NAMESPACE
}
#endif
#endif
