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
      throw PropListIException("DSetMemXferPropList::setBuffer",
		"H5Pset_buffer failed");
   }
}

// Reads buffer settings
size_t DSetMemXferPropList::getBuffer( void** tconv, void** bkg ) const
{
   size_t buffer_size = H5Pget_buffer( id, tconv, bkg );
   if( buffer_size == 0 )
   {
      throw PropListIException("DSetMemXferPropList::getBuffer",
		"H5Pget_buffer returned 0 for buffer size - failure");
   }
   return( buffer_size );
}

// Sets the dataset transfer property list status to TRUE or FALSE
void DSetMemXferPropList::setPreserve( bool status ) const
{
   herr_t ret_value = H5Pset_preserve( id, (hbool_t) status );
   if( ret_value < 0 )
   {
      throw PropListIException("DSetMemXferPropList::setPreserve",
		"H5Pset_preserve failed");
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
      throw PropListIException("DSetMemXferPropList::getPreserve",
		"H5Pget_preserve returned negative value for status");
   }
}

// Indicates whether to cache hyperslab blocks during I/O
void DSetMemXferPropList::setHyperCache( bool cache, unsigned limit ) const
{
   herr_t ret_value = H5Pset_hyper_cache( id, cache, limit );
   if( ret_value < 0 )
   {
      throw PropListIException("DSetMemXferPropList::setHyperCache",
		"H5Pset_hyper_cache failed");
   }
}

// Returns information regarding the caching of hyperslab blocks during I/O
void DSetMemXferPropList::getHyperCache( bool& cache, unsigned& limit ) const
{
   unsigned temp_cache;  // C routine takes hid_t, unsigned*, unsigned*
   herr_t ret_value = H5Pget_hyper_cache( id, &temp_cache, &limit );
   if( ret_value < 0 )
   {
      throw PropListIException("DSetMemXferPropList::getHyperCache",
		"H5Pget_hyper_cache failed");
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
      throw PropListIException("DSetMemXferPropList::setBtreeRatios",
		"H5Pset_btree_ratios failed");
   }
}

// Gets B-tree split ratios for a dataset transfer property list
void DSetMemXferPropList::getBtreeRatios( double& left, double& middle, double& right ) const
{
   herr_t ret_value = H5Pget_btree_ratios( id, &left, &middle, &right );
   if( ret_value < 0 )
   {
      throw PropListIException("DSetMemXferPropList::getBtreeRatios",
		"H5Pget_btree_ratios failed");
   }
}

// Sets the memory manager for variable-length datatype allocation
void DSetMemXferPropList::setVlenMemManager( H5MM_allocate_t alloc_func, void* alloc_info, H5MM_free_t free_func, void* free_info ) const
{
   herr_t ret_value = H5Pset_vlen_mem_manager( id, alloc_func, alloc_info, 
						free_func, free_info );
   if( ret_value < 0 )
   {
      throw PropListIException("DSetMemXferPropList::setVlenMemManager",
		"H5Pset_vlen_mem_manager failed");
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
      //throw PropListIException("DSetMemXferPropList::setVlenMemManager",
		//"H5Pset_vlen_mem_manager failed");
   //}
}

// Gets the memory manager for variable-length datatype allocation 
void DSetMemXferPropList::getVlenMemManager( H5MM_allocate_t& alloc_func, void** alloc_info, H5MM_free_t& free_func, void** free_info ) const
{
   herr_t ret_value = H5Pget_vlen_mem_manager( id, &alloc_func, alloc_info, &free_func, free_info );
   if( ret_value < 0 )
   {
      throw PropListIException("DSetMemXferPropList::getVlenMemManager",
		"H5Pget_vlen_mem_manager failed");
   }
}

/* this function is in parallel mode only - not supported at this time.
// Sets the transfer mode
void DSetMemXferPropList::setXfer( H5D_transfer_t data_xfer_mode = H5D_XFER_INDEPENDENT ) const
{
   herr_t ret_value = H5Pset_xfer( ... );
   if( ret_value < 0 )
   {
      throw PropListIException("DSetMemXferPropList::setXfer",
		"H5Pset_xfer failed");
   }
}

// this function is in parallel mode only - not supported at this time.
// Gets the transfer mode
H5D_transfer_t DSetMemXferPropList::getXfer() const
{
   H5D_transfer_t xfer = H5Pget_xfer( id );
// BMR - need to find out what the value is for ?? when this function 
// is supported
   if( xfer == ?? )
   {
      throw PropListIException("DSetMemXferPropList::getXfer",
		"H5Pget_xfer failed");
   }
   return( xfer );
}

*/

// Default destructor
DSetMemXferPropList::~DSetMemXferPropList() {}

#ifndef H5_NO_NAMESPACE
} // end namespace
#endif
