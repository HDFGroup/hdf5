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
#include "H5FcreatProp.h"

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

const FileCreatPropList FileCreatPropList::DEFAULT( H5P_DEFAULT );

// Creates a file create property list
FileCreatPropList::FileCreatPropList() : PropList( H5P_FILE_CREATE ) {}

// Copy constructor: makes a copy of the original FileCreatPropList object;
FileCreatPropList::FileCreatPropList( const FileCreatPropList& orig ) : PropList( orig ) {}

void FileCreatPropList::getVersion( 
			int& boot, int& freelist, int& stab, int& shhdr ) const
{
   herr_t ret_value = H5Pget_version( id, &boot, &freelist, &stab, &shhdr );
   if( ret_value < 0 )
   {
      throw PropListIException("FileCreatPropList::getVersion", 
		"H5Pget_version failed");
   }
}

void FileCreatPropList::setUserblock( hsize_t size ) const
{
   herr_t ret_value = H5Pset_userblock( id, size);
   if( ret_value < 0 )
   {
      throw PropListIException("FileCreatPropList::setUserblock", 
		"H5Pset_userblock failed");
   }
}

hsize_t FileCreatPropList::getUserblock() const
{
   hsize_t userblock_size;
   herr_t ret_value = H5Pget_userblock( id, &userblock_size );
   if( ret_value < 0 )
   {
      throw PropListIException("FileCreatPropList::getUserblock", 
		"H5Pget_userblock failed");
   }
   return( userblock_size );
}

void FileCreatPropList::setSizes( size_t sizeof_addr, size_t sizeof_size ) const
{
   herr_t ret_value = H5Pset_sizes( id, sizeof_addr, sizeof_size );
   if( ret_value < 0 )
   {
      throw PropListIException("FileCreatPropList::setSizes", 
		"H5Pset_sizes failed");
   }
}

void FileCreatPropList::getSizes( size_t& sizeof_addr, size_t& sizeof_size ) const
{
   herr_t ret_value = H5Pget_sizes( id, &sizeof_addr, &sizeof_size );
   if( ret_value < 0 )
   {
      throw PropListIException("FileCreatPropList::getSizes", 
		"H5Pget_sizes failed");
   }
}

void FileCreatPropList::setSymk( int ik, int lk ) const
{
   herr_t ret_value = H5Pset_sym_k( id, ik, lk );
   if( ret_value < 0 )
   {
      throw PropListIException("FileCreatPropList::setSymk", 
		"H5Pset_sym_k failed");
   }
}

void FileCreatPropList::getSymk( int& ik, int& lk ) const
{
   herr_t ret_value = H5Pget_sym_k( id, &ik, &lk );
   if( ret_value < 0 )
   {
      throw PropListIException("FileCreatPropList::getSymk", 
		"H5Pget_sym_k failed");
   }
}

void FileCreatPropList::setIstorek( int ik ) const
{
   herr_t ret_value = H5Pset_istore_k( id, ik );
   if( ret_value < 0 )
   {
      throw PropListIException("FileCreatPropList::setIstorek", 
		"H5Pset_istore_k failed");
   }
}
int FileCreatPropList::getIstorek() const
{
   int ik;
   herr_t ret_value = H5Pget_istore_k( id, &ik );
   if( ret_value < 0 )
   {
      throw PropListIException("FileCreatPropList::getIstorek", 
		"H5Pget_istore_k failed");
   }
   return( ik );
}

// Default destructor
FileCreatPropList::~FileCreatPropList() {}

#ifndef H5_NO_NAMESPACE
} // end namespace
#endif
