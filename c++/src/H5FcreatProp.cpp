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
      throw PropListIException();
   }
}

void FileCreatPropList::setUserblock( hsize_t size ) const
{
   herr_t ret_value = H5Pset_userblock( id, size);
   if( ret_value < 0 )
   {
      throw PropListIException();
   }
}

hsize_t FileCreatPropList::getUserblock() const
{
   hsize_t userblock_size;
   herr_t ret_value = H5Pget_userblock( id, &userblock_size );
   if( ret_value < 0 )
   {
      throw PropListIException();
   }
   return( userblock_size );
}

void FileCreatPropList::setSizes( size_t sizeof_addr, size_t sizeof_size ) const
{
   herr_t ret_value = H5Pset_sizes( id, sizeof_addr, sizeof_size );
   if( ret_value < 0 )
   {
      throw PropListIException();
   }
}

void FileCreatPropList::getSizes( size_t& sizeof_addr, size_t& sizeof_size ) const
{
   herr_t ret_value = H5Pget_sizes( id, &sizeof_addr, &sizeof_size );
   if( ret_value < 0 )
   {
      throw PropListIException();
   }
}

void FileCreatPropList::setSymk( int ik, int lk ) const
{
   herr_t ret_value = H5Pset_sym_k( id, ik, lk );
   if( ret_value < 0 )
   {
      throw PropListIException();
   }
}

void FileCreatPropList::getSymk( int& ik, int& lk ) const
{
   herr_t ret_value = H5Pget_sym_k( id, &ik, &lk );
   if( ret_value < 0 )
   {
      throw PropListIException();
   }
}

void FileCreatPropList::setIstorek( int ik ) const
{
   herr_t ret_value = H5Pset_istore_k( id, ik );
   if( ret_value < 0 )
   {
      throw PropListIException();
   }
}
int FileCreatPropList::getIstorek() const
{
   int ik;
   herr_t ret_value = H5Pget_istore_k( id, &ik );
   if( ret_value < 0 )
   {
      throw PropListIException();
   }
   return( ik );
}

// Default destructor
FileCreatPropList::~FileCreatPropList() {}

#ifndef H5_NO_NAMESPACE
} // end namespace
#endif
