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

//--------------------------------------------------------------------------
// Function	Default constructor
///\brief	Default constructor - Creates a stub dataset memory and 
///		transfer property list object.
// Programmer:	Binh-Minh Ribler
//--------------------------------------------------------------------------
DSetMemXferPropList::DSetMemXferPropList() : PropList(H5P_DATASET_XFER) {}

//--------------------------------------------------------------------------
// Function	Copy constructor
///\brief	Copy constructor - makes a copy of the original 
///		DSetMemXferPropList object
///\param	orig - IN: The original dataset memory and transfer property 
///				list object to copy
// Programmer:	Binh-Minh Ribler
//--------------------------------------------------------------------------
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

// Sets an exception handling callback for datatype conversion
// for a dataset transfer property list
void DSetMemXferPropList::setTypeConvCB( H5T_conv_except_func_t op, void *user_data) const
{
   herr_t ret_value = H5Pset_type_conv_cb( id, op, user_data);
   if( ret_value < 0 )
   {
      throw PropListIException("DSetMemXferPropList::setTypeConvCB",
                "H5Pset_type_conv_cb failed");
   }
}

// Sets an exception handling callback for datatype conversion
// for a dataset transfer property list
void DSetMemXferPropList::getTypeConvCB( H5T_conv_except_func_t *op, void **user_data) const
{
   herr_t ret_value = H5Pget_type_conv_cb( id, op, user_data);
   if( ret_value < 0 )
   {
      throw PropListIException("DSetMemXferPropList::getTypeConvCB",
                "H5Pget_type_conv_cb failed");
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

//--------------------------------------------------------------------------
// Function:    DSetMemXferPropList::setMulti
///\brief       Sets the data transfer property list for the multi-file driver.
///\param       memb_dxpl - OUT: Array of data access property lists
///\exception   H5::PropListIException
///\par Description
///		This function can only be used after the member map has 
///		been set with FileAccPropList::setMulti (not done - BMR.)
// Programmer:	Binh-Minh Ribler - April, 2004
//--------------------------------------------------------------------------
void DSetMemXferPropList::setMulti(const hid_t *memb_dxpl)
{
   herr_t ret_value = H5Pset_dxpl_multi(id, memb_dxpl);
   if (ret_value < 0)
   {
      throw PropListIException("DSetMemXferPropList::setMulti",
		"H5Pset_dxpl_multi failed");
   }
}

//--------------------------------------------------------------------------
// Function:    DSetMemXferPropList::getMulti
///\brief       Returns multi-file data transfer property list information.
///\param       memb_dxpl - OUT: Array of data access property lists
///\exception   H5::PropListIException
// Programmer:	Binh-Minh Ribler - April, 2004
//--------------------------------------------------------------------------
void DSetMemXferPropList::getMulti(hid_t *memb_dxpl)
{
   herr_t ret_value = H5Pget_dxpl_multi(id, memb_dxpl);
   if (ret_value < 0)
   {
      throw PropListIException("DSetMemXferPropList::getMulti",
		"H5Pget_dxpl_multi failed");
   }
}

//--------------------------------------------------------------------------
// Function:    DSetMemXferPropList::setSmallDataBlockSize
///\brief       Sets the size of a contiguous block reserved for small data.
///\param       size - IN: Maximum size, in bytes, of the small data block. 
///\exception   H5::PropListIException
///\par Description
///		For detail, please refer to the C layer Reference Manual at:
/// http://hdf.ncsa.uiuc.edu/HDF5/doc/RM_H5P.html#Property-SetSmallData
// Programmer:	Binh-Minh Ribler - April, 2004
//--------------------------------------------------------------------------
void DSetMemXferPropList::setSmallDataBlockSize(hsize_t size)
{
   herr_t ret_value = H5Pset_small_data_block_size(id, size);
   if (ret_value < 0)
   {
      throw PropListIException("DSetMemXferPropList::setSmallDataBlockSize",
		"H5Pset_small_data_block_size failed");
   }
}

//--------------------------------------------------------------------------
// Function:    DSetMemXferPropList::getSmallDataBlockSize
///\brief	Returns the current small data block size setting.
///\return	Size of the small data block, in bytes
///\exception   H5::PropListIException
// Programmer:	Binh-Minh Ribler - April, 2004
//--------------------------------------------------------------------------
hsize_t DSetMemXferPropList::getSmallDataBlockSize()
{
   hsize_t size;
   herr_t ret_value = H5Pget_small_data_block_size(id, &size);
   if (ret_value < 0)
   {
      throw PropListIException("DSetMemXferPropList::getSmallDataBlockSize",
		"H5Pget_small_data_block_size failed");
   }
   return(size);
}

//--------------------------------------------------------------------------
// Function:    DSetMemXferPropList::setHyperVectorSize
///\brief	Sets number of I/O vectors to be read/written in hyperslab I/O.
///\exception   H5::PropListIException
///\par Description
///		For information, please refer to the C layer Reference
///		Manual at:
/// http://hdf.ncsa.uiuc.edu/HDF5/doc/RM_H5P.html#Property-SetHyperVectorSize
// Programmer:	Binh-Minh Ribler - April, 2004
//--------------------------------------------------------------------------
void DSetMemXferPropList::setHyperVectorSize(size_t vector_size)
{
   herr_t ret_value = H5Pset_hyper_vector_size(id, vector_size);
   if (ret_value < 0)
   {
      throw PropListIException("DSetMemXferPropList::setHyperVectorSize",
		"H5Pset_hyper_vector_size failed");
   }
}

//--------------------------------------------------------------------------
// Function:    DSetMemXferPropList::getSmallDataBlockSize
///\brief	Returns the number of I/O vectors to be read/written in 
///		hyperslab I/O.
///\return	Number of I/O vectors
///\exception   H5::PropListIException
// Programmer:	Binh-Minh Ribler - April, 2004
//--------------------------------------------------------------------------
size_t DSetMemXferPropList::getHyperVectorSize()
{
   size_t vector_size;
   herr_t ret_value = H5Pget_hyper_vector_size(id, &vector_size);
   if (ret_value < 0)
   {
      throw PropListIException("DSetMemXferPropList::getHyperVectorSize",
		"H5Pget_hyper_vector_size failed");
   }
   return(vector_size);
}

//--------------------------------------------------------------------------
// Function:    DSetMemXferPropList::setEDCCheck
///\brief	Enables or disables error-detecting for a dataset reading
///		process.
///\param	check - IN: Specifies whether error detection is enabled or
///				disabled
///\exception   H5::PropListIException
///\par Description
///		The error detection algorithm used is the algorithm previously 
///		specified in the corresponding dataset creation property 
///		list.  This function does not affect the use of error 
///		detection in the writing process.
///\par
///		Valid values are as follows:
///		\li \c H5Z_ENABLE_EDC   (default) 
///		\li \c H5Z_DISABLE_EDC  
// Programmer:	Binh-Minh Ribler - April, 2004
//--------------------------------------------------------------------------
void DSetMemXferPropList::setEDCCheck(H5Z_EDC_t check)
{
   herr_t ret_value = H5Pset_edc_check(id, check);
   if (ret_value < 0)
   {
      throw PropListIException("DSetMemXferPropList::setEDCCheck",
		"H5Pset_edc_check failed");
   }
}

//--------------------------------------------------------------------------
// Function:    DSetMemXferPropList::getEDCCheck
///\brief	Determines whether error-detection is enabled for dataset reads.
///\return	\c H5Z_ENABLE_EDC or \c H5Z_DISABLE_EDC
///\exception   H5::PropListIException
// Programmer:	Binh-Minh Ribler - April, 2004
//--------------------------------------------------------------------------
H5Z_EDC_t DSetMemXferPropList::getEDCCheck()
{
   H5Z_EDC_t check = H5Pget_edc_check(id);
   if (check < 0)
   {
      throw PropListIException("DSetMemXferPropList::getEDCCheck",
		"H5Pget_edc_check failed");
   }
   return(check);
}

// Default destructor
DSetMemXferPropList::~DSetMemXferPropList() {}

#ifndef H5_NO_NAMESPACE
} // end namespace
#endif

