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
#include "H5Idtemplates.h"
#include "H5PropList.h"
#include "H5Object.h"
#include "H5PropList.h"
#include "H5DxferProp.h"
#include "H5DataType.h"
#include "H5DcreatProp.h"
#include "H5DataSpace.h"
#include "H5AbstractDs.h"
#include "H5DataSet.h"

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

// Default constructor
DataSet::DataSet() : AbstractDs() {}

// Creates a copy of DataSet using an existing id
DataSet::DataSet( const hid_t dataset_id ) : AbstractDs( dataset_id ) {}

// Copy constructor makes a copy of the original object by using base
// class' copy constructors
DataSet::DataSet( const DataSet& original ) : AbstractDs( original ) {}

// Gets a copy of the dataspace of this dataset
DataSpace DataSet::getSpace() const
{
   // Calls C function H5Dget_space to get the id of the dataspace
   hid_t dataspace_id = H5Dget_space( id );

   // If the dataspace id is invalid, throw an exception
   if( dataspace_id <= 0 )
   {
      throw DataSetIException("DataSet::getSpace", "H5Dget_space failed");
   }
   //create dataspace object using the existing id then return the object
   DataSpace data_space( dataspace_id );
   return( data_space );
}

// This private member function calls the C API to get the identifier 
// of the datatype that is used by this dataset.  It is used
// by the various AbstractDs functions to get the specific datatype.
hid_t DataSet::p_getType() const
{
   hid_t type_id = H5Dget_type( id );
   if( type_id > 0 )
      return( type_id );
   else
   {
      throw DataSetIException(NULL, "H5Dget_type failed");
   }
}

// Gets the dataset creation property list
DSetCreatPropList DataSet::getCreatePlist() const
{
   hid_t create_plist_id = H5Dget_create_plist( id );
   if( create_plist_id <= 0 )
   {
      throw DataSetIException("DataSet::getCreatePlist", "H5Dget_create_plist failed");
   }
   // create and return the DSetCreatPropList object 
   DSetCreatPropList create_plist( create_plist_id );
   return( create_plist );
}

// Returns the amount of storage required for a dataset.  
hsize_t DataSet::getStorageSize() const
{
   hsize_t storage_size = H5Dget_storage_size( id );

   if( storage_size > 0 )  // checking with Quincey for failure value - BMR
      return( storage_size );
   else
   {
      throw DataSetIException("DataSet::getStorageSize", "H5Dget_storage_size failed");
   }
}

// Returns the number of bytes required to store VL data.
hsize_t DataSet::getVlenBufSize( DataType& type, DataSpace& space ) const
{
   // Obtain identifiers for C API
   //hid_t type_id = type.getId();
   //hid_t space_id = space.getId();
   //hsize_t size;

   //herr_t ret_value = H5Dget_vlen_buf_size( id, type_id, space_id, &size );
   //if( ret_value >= 0 )
   //   return( size );
   //else
   //{
      //throw DataSetIException();
   //}
   throw DataSetIException( "DataSet::getVlenBufSize", 
		"Currently not implemented yet.");
   return (0);
}

// Reclaims VL datatype memory buffers. 
void DataSet::vlenReclaim( DataType& type, DataSpace& space, DSetMemXferPropList& xfer_plist, void* buf ) const
{
   // Obtain identifiers for C API
   hid_t type_id = type.getId();
   hid_t space_id = space.getId();
   hid_t xfer_plist_id = xfer_plist.getId();

   herr_t ret_value = H5Dvlen_reclaim( type_id, space_id, xfer_plist_id, buf );
   if( ret_value < 0 )
   {
      throw DataSetIException("DataSet::vlenReclaim", "H5Dvlen_reclaim failed");
   }
}

// Reads raw data from the specified dataset into buf, converting from 
// file datatype and dataspace to memory datatype and dataspace. 
void DataSet::read( void* buf, const DataType& mem_type, const DataSpace& mem_space, const DataSpace& file_space, const DSetMemXferPropList& xfer_plist ) const
{
   // Obtain identifiers for C API
   hid_t mem_type_id = mem_type.getId();
   hid_t mem_space_id = mem_space.getId();
   hid_t file_space_id = file_space.getId();
   hid_t xfer_plist_id = xfer_plist.getId();

   herr_t ret_value = H5Dread( id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, buf );
   if( ret_value < 0 )
   {
      throw DataSetIException("DataSet::read", "H5Dread failed");
   }
}

// Writes raw data from an application buffer buffer to a dataset, 
// converting from memory datatype and dataspace to file datatype 
// and dataspace.
void DataSet::write( const void* buf, const DataType& mem_type, const DataSpace& mem_space, const DataSpace& file_space, const DSetMemXferPropList& xfer_plist ) const
{
   // Obtain identifiers for C API
   hid_t mem_type_id = mem_type.getId();
   hid_t mem_space_id = mem_space.getId();
   hid_t file_space_id = file_space.getId();
   hid_t xfer_plist_id = xfer_plist.getId();

   herr_t ret_value = H5Dwrite( id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, buf );
   if( ret_value < 0 )
   {
      throw DataSetIException("DataSet::write", "H5Dwrite failed");
   }
}

// Iterates over all selected elements in a dataspace. 
int DataSet::iterateElems( void* buf, const DataType& type, const DataSpace& space, H5D_operator_t op, void* op_data )
{
   // Obtain identifiers for C API
   hid_t type_id = type.getId();
   hid_t space_id = space.getId();
   herr_t ret_value = H5Diterate( buf, type_id, space_id, op, op_data );
   if( ret_value >= 0 )
      return( ret_value );
   else  // raise exception when H5Diterate returns a negative value
   {
      throw DataSetIException("DataSet::iterateElems", "H5Diterate failed");
   }
}

// Extends a dataset with unlimited dimension.
void DataSet::extend( const hsize_t* size ) const
{
   herr_t ret_value = H5Dextend( id, size );
   if( ret_value < 0 )  // raise exception when H5Dextend returns a neg value
   {
      throw DataSetIException("DataSet::extend", "H5Dextend failed");
   }
}

// This private function calls the C API H5Dclose to close this dataset.
// Used by IdComponent::reset
void DataSet::p_close() const
{
   herr_t ret_value = H5Dclose( id );
   if( ret_value < 0 )
   {
      throw DataSetIException(NULL, "H5Dclose failed");
   }
}

// The destructor of this instance calls IdComponent::reset to
// reset its identifier - no longer true
// Older compilers (baldric) don't support template member functions
// and IdComponent::reset is one; so at this time, the resetId is not
// a member function so it can be template to work around that problem.
DataSet::~DataSet()
{
   // The dataset id will be closed properly 
    try {
	resetIdComponent( this ); }
    catch (Exception close_error) { // thrown by p_close
	throw DataSetIException("DataSet::~DataSet", close_error.getDetailMsg());
    }
}

#ifndef H5_NO_NAMESPACE
} // end namespace
#endif
