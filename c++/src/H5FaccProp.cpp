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
#include "H5PropList.h"
#include "H5FaccProp.h"

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

const FileAccPropList FileAccPropList::DEFAULT( H5P_DEFAULT );

// Creates a file access property list
FileAccPropList::FileAccPropList() : PropList( H5P_FILE_ACCESS ) {}

// Copy constructor: makes a copy of the original FileAccPropList object;
FileAccPropList::FileAccPropList( const FileAccPropList& orig ) : PropList( orig ) {}

/* commented out for 1.3.x, only in 1.2.x
void FileAccPropList::setStdio() const
{
   herr_t ret_value = H5Pset_stdio( id );
   if( ret_value < 0 )
   {
      throw PropListIException("FileAccPropList::setStdio", "H5Pset_stdio failed");
   }
}

bool FileAccPropList::getStdio() const
{
   herr_t ret_value = H5Pget_stdio( id );
   if( ret_value < 0 )
      return( false );
   else
      return( true );
}

H5F_driver_t FileAccPropList::getDriver() const
{
   H5F_driver_t driver = H5Pget_driver( id );
   if( driver == H5F_LOW_ERROR )
   {
      throw PropListIException("FileAccPropList::getDriver", "H5Pget_driver failed");
   }
   return( driver );
}

void FileAccPropList::setSec2() const
{
   herr_t ret_value = H5Pset_sec2( id );
   if( ret_value < 0 )
   {
      throw PropListIException("FileAccPropList::setSec2", "H5Pset_sec2 failed");
   }
}

bool FileAccPropList::getSec2() const
{
   herr_t ret_value = H5Pget_sec2( id );
   if( ret_value < 0 )
      return( false );
   else
      return( true );
}

void FileAccPropList::setCore( size_t increment ) const
{
   herr_t ret_value = H5Pset_core( id, increment );
   if( ret_value < 0 )
   {
      throw PropListIException("FileAccPropList::setCore", "H5Pset_core failed");
   }
}

bool FileAccPropList::getCore( size_t& increment) const
{
   herr_t ret_value = H5Pget_core( id, &increment );
   if( ret_value < 0 )
      return( false );
   else
      return( true );
}

void FileAccPropList::setFamily( hsize_t memb_size, const FileAccPropList& memb_plist ) const
{
   herr_t ret_value = H5Pset_family( id, memb_size, memb_plist.getId() );
   if( ret_value < 0 )
   {
      throw PropListIException("FileAccPropList::setFamily", "H5Pset_family failed");
   }
}

//Note: working on this return value here.  added copy constructor
//that uses PropList copy const. but din't work
// Determines whether the file access property list is set to the family
// driver then retrieves the family member's property list and returns
// true or false
bool FileAccPropList::getFamily( hsize_t& memb_size, FileAccPropList& memb_plist ) const
{
   hid_t memb_plist_id;
   herr_t ret_value = H5Pget_family( id, &memb_size, &memb_plist_id );
   if( ret_value < 0 )
   {
      memb_plist.setId( 0 );
      return( false );
   }
   else
   {
      memb_plist.setId( memb_plist_id );
      return( true );
   }
}

void FileAccPropList::setSplit( FileAccPropList& meta_plist, FileAccPropList& raw_plist, const char* meta_ext, const char* raw_ext ) const
{
   hid_t meta_pid = meta_plist.getId();
   hid_t raw_pid = raw_plist.getId();
   herr_t ret_value = H5Pset_split( id, meta_ext, meta_pid, raw_ext, raw_pid );
   if( ret_value < 0 )
   {
      throw PropListIException("FileAccPropList::setSplit", "H5Pset_split failed");
   }
}

void FileAccPropList::setSplit( FileAccPropList& meta_plist, FileAccPropList& raw_plist, const string& meta_ext, const string& raw_ext ) const
{
   setSplit( meta_plist, raw_plist, meta_ext.c_str(), raw_ext.c_str() );
}

void FileAccPropList::getSplit( size_t meta_ext_size, string& meta_ext, FileAccPropList& meta_plist, size_t raw_ext_size, string& raw_ext, FileAccPropList& raw_plist ) const
{
   hid_t meta_plist_id, raw_plist_id;	// meta-data and raw-data plist ids
   char* meta_ext_C = new char[meta_ext_size];	// meta-data extension in C
   char* raw_ext_C = new char[raw_ext_size];	// raw-data extension in C
   herr_t ret_value = H5Pget_split( id, meta_ext_size, meta_ext_C, 
		&meta_plist_id, raw_ext_size, raw_ext_C, &raw_plist_id );
   if( ret_value < 0 )
   {
      throw PropListIException("FileAccPropList::getSplit", "H5Pget_split failed");
   }
   meta_plist.setId( meta_plist_id );
   raw_plist.setId( raw_plist_id );
   raw_ext = string( raw_ext_C );
   meta_ext = string( raw_ext_C );
   delete [] raw_ext_C;
   delete [] meta_ext_C;
}
*/

void FileAccPropList::setAlignment( hsize_t threshold, hsize_t alignment ) const
{
   herr_t ret_value = H5Pset_alignment( id, threshold, alignment );
   if( ret_value < 0 )
   {
      throw PropListIException("FileAccPropList::setAlignment", "H5Pset_alignment failed");
   }
}

void FileAccPropList::getAlignment( hsize_t& threshold, hsize_t& alignment ) const
{
   herr_t ret_value = H5Pget_alignment( id, &threshold, &alignment );
   if( ret_value < 0 )
   {
      throw PropListIException("FileAccPropList::getAlignment", "H5Pget_alignment failed");
   }
}

/* MPI_Comm and MPI_Info not declared in serial mode so leave these
routines out until C++ API needs to deal with parallel
void FileAccPropList::setMpi( MPI_Comm comm, MPI_Info info ) const
{
   herr_t ret_value = H5Pset_mpi( id, comm, info );
   if( ret_value < 0 )
   {
      throw PropListIException("FileAccPropList::setMpi", "H5Pset_mpi failed");
   }
}

void FileAccPropList::getMpi( MPI_Comm& comm, MPI_Info& info ) const
{
   herr_t ret_value = H5Pget_mpi( id, &comm, &info );
   if( ret_value < 0 )
      return( false );
   else
      return( true );
}
*/

void FileAccPropList::setCache( int mdc_nelmts, int rdcc_nelmts, size_t rdcc_nbytes, double rdcc_w0 ) const
{
   herr_t ret_value = H5Pset_cache( id, mdc_nelmts, rdcc_nelmts, rdcc_nbytes, rdcc_w0 );
   if( ret_value < 0 )
   {
      throw PropListIException("FileAccPropList::setCache", "H5Pset_cache failed");
   }
}

void FileAccPropList::getCache( int& mdc_nelmts, int& rdcc_nelmts, size_t& rdcc_nbytes, double& rdcc_w0 ) const
{
   herr_t ret_value = H5Pget_cache( id, &mdc_nelmts, &rdcc_nelmts, &rdcc_nbytes, &rdcc_w0 );
   if( ret_value < 0 )
   {
      throw PropListIException("FileAccPropList::getCache", "H5Pget_cache failed");
   }
}

void FileAccPropList::setGcReferences( unsigned gc_ref ) const
{
   herr_t ret_value = H5Pset_gc_references( id, gc_ref );
   if( ret_value < 0 )
   {
      throw PropListIException("FileAccPropList::setGcReferences", "H5Pset_gc_references failed");
   }
}

unsigned FileAccPropList::getGcReferences() const
{
   unsigned gc_ref;

   // the name of this routine will be changed to H5Pget_gc_references???
   herr_t ret_value = H5Pget_gc_references( id, &gc_ref );
   if( ret_value < 0 )
   {
      throw PropListIException("FileAccPropList::getGcReferences", "H5Pget_gc_references failed");
   }
   return( gc_ref );
}

FileAccPropList::~FileAccPropList() {}

#ifndef H5_NO_NAMESPACE
} // end namespace
#endif
