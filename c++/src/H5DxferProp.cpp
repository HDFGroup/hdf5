#include <string>

#include "H5Include.h"
#include "H5RefCounter.h"
#include "H5Exception.h"
#include "H5IdComponent.h"
#include "H5PropList.h"
#include "H5DxferProp.h"

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

const DSetMemXferPropList DSetMemXferPropList::DEFAULT( H5P_DEFAULT );

// Creates a dataset memory and transfer property list
DSetMemXferPropList::DSetMemXferPropList() : PropList( H5P_DATASET_XFER ) {}

// Copy constructor: makes a copy of the original DSetMemXferPropList object;
DSetMemXferPropList::DSetMemXferPropList( const DSetMemXferPropList& orig ) : PropList( orig ) {}

// Sets type conversion and background buffers
void DSetMemXferPropList::setBuffer( size_t size, void* tconv, void* bkg ) const
{
   herr_t ret_value = H5Pset_buffer( id, size, tconv, bkg );
   if( ret_value < 0 )
   {
      throw PropListIException();
   }
}

// Reads buffer settings
size_t DSetMemXferPropList::getBuffer( void** tconv, void** bkg ) const
{
   size_t buffer_size = H5Pget_buffer( id, tconv, bkg );
   if( buffer_size == 0 )
   {
      throw PropListIException();
   }
   return( buffer_size );
}

// Sets the dataset transfer property list status to TRUE or FALSE
void DSetMemXferPropList::setPreserve( bool status ) const
{
   herr_t ret_value = H5Pset_preserve( id, (hbool_t) status );
   if( ret_value < 0 )
   {
      throw PropListIException();
   }
}

// Checks status of the dataset transfer property list
bool DSetMemXferPropList::getPreserve() const
{
   int ret_value = H5Pget_preserve( id );
   if( ret_value > 0 )
      return true;
   else if( ret_value == 0 )
      return false;
   else
   {
      throw PropListIException();
   }
}

// Indicates whether to cache hyperslab blocks during I/O
void DSetMemXferPropList::setHyperCache( bool cache, unsigned limit ) const
{
   herr_t ret_value = H5Pset_hyper_cache( id, cache, limit );
   if( ret_value < 0 )
   {
      throw PropListIException();
   }
}

// Returns information regarding the caching of hyperslab blocks during I/O
void DSetMemXferPropList::getHyperCache( bool& cache, unsigned& limit ) const
{
   unsigned temp_cache;  // C routine takes hid_t, unsigned*, unsigned*
   herr_t ret_value = H5Pget_hyper_cache( id, &temp_cache, &limit );
   if( ret_value < 0 )
   {
      throw PropListIException();
   }
   if( temp_cache > 0 )
      cache = true;
   else
      cache = false;
}

// Sets B-tree split ratios for a dataset transfer property list 
void DSetMemXferPropList::setBtreeRatios( double left, double middle, double right ) const
{
   herr_t ret_value = H5Pset_btree_ratios( id, left, middle, right );
   if( ret_value < 0 )
   {
      throw PropListIException();
   }
}

// Gets B-tree split ratios for a dataset transfer property list
void DSetMemXferPropList::getBtreeRatios( double& left, double& middle, double& right ) const
{
   herr_t ret_value = H5Pget_btree_ratios( id, &left, &middle, &right );
   if( ret_value < 0 )
   {
      throw PropListIException();
   }
}

// Sets the memory manager for variable-length datatype allocation
void DSetMemXferPropList::setVlenMemManager( H5MM_allocate_t alloc_func, void* alloc_info, H5MM_free_t free_func, void* free_info ) const
{
   herr_t ret_value = H5Pset_vlen_mem_manager( id, alloc_func, alloc_info, 
						free_func, free_info );
   if( ret_value < 0 )
   {
      throw PropListIException();
   }
}

// alloc_func and free_func are set to NULL, indicating that system malloc and 
// free are to be used
void DSetMemXferPropList::setVlenMemManager() const
{
   setVlenMemManager( NULL, NULL, NULL, NULL );
   //herr_t ret_value = H5Pset_vlen_mem_manager( id, NULL, NULL, NULL, NULL );
   //if( ret_value < 0 )
   //{
      //throw PropListIException();
   //}
}

// Gets the memory manager for variable-length datatype allocation 
void DSetMemXferPropList::getVlenMemManager( H5MM_allocate_t& alloc_func, void** alloc_info, H5MM_free_t& free_func, void** free_info ) const
{
   herr_t ret_value = H5Pget_vlen_mem_manager( id, &alloc_func, alloc_info, &free_func, free_info );
   if( ret_value < 0 )
   {
      throw PropListIException();
   }
}

/* this function is in parallel mode only - not supported at this time.
// Sets the transfer mode
void DSetMemXferPropList::setXfer( H5D_transfer_t data_xfer_mode = H5D_XFER_INDEPENDENT ) const
{
   herr_t ret_value = H5Pset_xfer( ... );
   if( ret_value < 0 )
   {
      throw PropListIException();
   }
}

// this function is in parallel mode only - not supported at this time.
// Gets the transfer mode
H5D_transfer_t DSetMemXferPropList::getXfer() const
{
   H5D_transfer_t xfer = H5Pget_xfer( id );
// remove when done - find out what the value is for ??
   if( xfer == ?? )
   {
      throw PropListIException();
   }
   return( xfer );
}

*/

// Default destructor
DSetMemXferPropList::~DSetMemXferPropList() {}

#ifndef H5_NO_NAMESPACE
} // end namespace
#endif
