#include <string>

#include "H5Include.h"
#include "H5Exception.h"
#include "H5Library.h"

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

// This static variable will be set to true when dontAtExit is called
bool H5Library::need_cleanup = false;

// Initializes the HDF5 library. 
void H5Library::open()
{
   herr_t ret_value = H5open();
   if( ret_value < 0 )
   {
      throw LibraryIException("H5Library::open", "H5open failed");
   }
}

// Flushes all data to disk, closes files, and cleans up memory. 
void H5Library::close()
{
   herr_t ret_value = H5close();
   if( ret_value < 0 )
   {
      throw LibraryIException("H5Library::close", "H5close failed");
   }
}

// Instructs library not to install atexit cleanup routine
void H5Library::dontAtExit()
{
   herr_t ret_value = H5dont_atexit();
   if( ret_value < 0 )
   {
      throw LibraryIException("H5Library::dontAtExit", "H5dont_atexit failed");
   }
}

// Returns the HDF library release number. 
void H5Library::getLibVersion( unsigned& majnum, unsigned& minnum, unsigned& relnum )
{
   herr_t ret_value = H5get_libversion( &majnum, &minnum, &relnum );
   if( ret_value < 0 )
   {
      throw LibraryIException("H5Library::getLibVersion", "H5get_libversion failed");
   }
}

// Verifies that the arguments match the version numbers compiled
// into the library
void H5Library::checkVersion( unsigned majnum, unsigned minnum, unsigned relnum )
{
   herr_t ret_value = H5check_version( majnum, minnum, relnum  );
   if( ret_value < 0 )
   {
      throw LibraryIException("H5Library::checkVersion", "H5check_version failed");
   }
}

#ifndef H5_NO_NAMESPACE
} // end namespace
#endif
