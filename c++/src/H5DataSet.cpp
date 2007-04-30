/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#ifdef OLD_HEADER_FILENAME
#include <iostream.h>
#else
#include <iostream>
#endif
#include <string>

#include "H5Include.h"
#include "H5Exception.h"
#include "H5IdComponent.h"
#include "H5PropList.h"
#include "H5Object.h"
#include "H5PropList.h"
#include "H5DxferProp.h"
#include "H5DcreatProp.h"
#include "H5CommonFG.h"
#include "H5DataType.h"
#include "H5DataSpace.h"
#include "H5AbstractDs.h"
#include "H5DataSet.h"

#ifndef H5_NO_NAMESPACE
namespace H5 {
#ifndef H5_NO_STD
    using std::cerr;
    using std::endl;
#endif  // H5_NO_STD
#endif

//--------------------------------------------------------------------------
// Function:	DataSet default constructor
///\brief	Default constructor: creates a stub DataSet.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
DataSet::DataSet() : AbstractDs() {}

//--------------------------------------------------------------------------
// Function:	DataSet overloaded constructor
///\brief	Creates an DataSet object using the id of an existing dataset.
///\param	existing_id - IN: Id of an existing dataset
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
DataSet::DataSet(const hid_t existing_id) : AbstractDs(existing_id) {}

//--------------------------------------------------------------------------
// Function:	DataSet copy constructor
///\brief	Copy constructor: makes a copy of the original DataSet object.
///\param	original - IN: DataSet instance to copy
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
DataSet::DataSet( const DataSet& original ) : AbstractDs( original ) {}

//--------------------------------------------------------------------------
// Function:	DataSet overload constructor - dereference
///\brief	Given a reference to some object, returns that dataset
///\param	obj - IN: Dataset reference object is in or location of
///                   object that the dataset is located within.
///\param	ref - IN: Reference pointer
///\exception	H5::DataSetIException
///\par Description
///		\c obj can be DataSet, Group, H5File, or named DataType, that 
///		is a datatype that has been named by DataType::commit.
// Programmer	Binh-Minh Ribler - Oct, 2006
//--------------------------------------------------------------------------
DataSet::DataSet(IdComponent& obj, void* ref) : AbstractDs()
{
   IdComponent::dereference(obj, ref);
}

//--------------------------------------------------------------------------
// Function:	DataSet::getSpace
///\brief	Gets a copy of the dataspace of this dataset.
///\return	DataSpace instance
///\exception	H5::DataSetIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
DataSpace DataSet::getSpace() const
{
   // Calls C function H5Dget_space to get the id of the dataspace
   hid_t dataspace_id = H5Dget_space( id );

   // If the dataspace id is invalid, throw an exception
   if( dataspace_id < 0 )
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
hid_t DataSet::p_get_type() const
{
   hid_t type_id = H5Dget_type( id );
   if( type_id > 0 )
      return( type_id );
   else
   {
      throw DataSetIException("", "H5Dget_type failed");
   }
}

//--------------------------------------------------------------------------
// Function:	DataSet::getCreatePlist
///\brief	Gets the dataset creation property list.
///\return	DSetCreatPropList instance
///\exception	H5::DataSetIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
DSetCreatPropList DataSet::getCreatePlist() const
{
   hid_t create_plist_id = H5Dget_create_plist( id );
   if( create_plist_id < 0 )
   {
      throw DataSetIException("DataSet::getCreatePlist", "H5Dget_create_plist failed");
   }
   // create and return the DSetCreatPropList object
   DSetCreatPropList create_plist( create_plist_id );
   return( create_plist );
}

//--------------------------------------------------------------------------
// Function:	DataSet::getStorageSize
///\brief	Returns the amount of storage required for a dataset.
///\return	Size of the storage or 0, for no data
///\exception	H5::DataSetIException
// Note:	H5Dget_storage_size returns 0 when there is no data.  This
//		function should have no failure. (from SLU)
// Programmer	Binh-Minh Ribler - Mar, 2005
//--------------------------------------------------------------------------
hsize_t DataSet::getStorageSize() const
{
   hsize_t storage_size = H5Dget_storage_size(id);
   return(storage_size);
}

//--------------------------------------------------------------------------
// Function:	DataSet::getOffset
///\brief	Returns the address of this dataset in the file.
///\return	Address of dataset
///\exception	H5::DataSetIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
haddr_t DataSet::getOffset() const
{
   haddr_t ds_addr; // for address of dataset

   ds_addr = H5Dget_offset(id);
   if( ds_addr == HADDR_UNDEF )
   {
      throw DataSetIException("DataSet::getOffset", "H5Dget_offset returned HADDR_UNDEF");
   }
   return(ds_addr);
}

//--------------------------------------------------------------------------
// Function:	DataSet::getSpaceStatus
///\brief	Determines whether space has been allocated for a dataset.
///\param	status - OUT: Space allocation status
///\exception	H5::DataSetIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void DataSet::getSpaceStatus(H5D_space_status_t& status) const
{
   herr_t ret_value = H5Dget_space_status(id, &status);
   if( ret_value < 0 )
   {
      throw DataSetIException("DataSet::getSpaceStatus", "H5Dget_space_status failed");
   }
}

//--------------------------------------------------------------------------
// Function:	DataSet::getVlenBufSize
///\brief	Returns the number of bytes required to store VL data.
///\return	Amount of storage
///\exception	H5::DataSetIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
hsize_t DataSet::getVlenBufSize( DataType& type, DataSpace& space ) const
{
   // Obtain identifiers for C API
   hid_t type_id = type.getId();
   hid_t space_id = space.getId();

   hsize_t size; // for amount of storage

   herr_t ret_value = H5Dvlen_get_buf_size( id, type_id, space_id, &size );
   if( ret_value < 0 )
   {
      throw DataSetIException("DataSet::getVlenBufSize", "H5Dvlen_get_buf_size failed");
   }
   return( size );
}

//--------------------------------------------------------------------------
// Function:	DataSet::vlenReclaim
///\brief	Reclaims VL datatype memory buffers.
///\param	type - IN: Datatype, which is the datatype stored in the buffer
///\param	space - IN: Selection for the memory buffer to free the
///		VL datatypes within
///\param	xfer_plist - IN: Property list used to create the buffer
///\param	buf - IN: Pointer to the buffer to be reclaimed
///\exception	H5::DataSetIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void DataSet::vlenReclaim(const DataType& type, const DataSpace& space, const DSetMemXferPropList& xfer_plist, void* buf )
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

//--------------------------------------------------------------------------
// Function:	DataSet::vlenReclaim
///\brief	Reclaims VL datatype memory buffers.
///\param	type - IN: Datatype, which is the datatype stored in the buffer
///\param	space - IN: Selection for the memory buffer to free the
///		VL datatypes within
///\param	xfer_plist - IN: Property list used to create the buffer
///\param	buf - IN: Pointer to the buffer to be reclaimed
///\exception	H5::DataSetIException
// Programmer	Binh-Minh Ribler - 2000
//\parDescription
//		This function has better prototype for the users than the
//		other, which might be removed at some point. BMR - 2006/12/20
//--------------------------------------------------------------------------
void DataSet::vlenReclaim(void* buf, const DataType& type, const DataSpace& space, const DSetMemXferPropList& xfer_plist)
{
   // Obtain identifiers for C API
   hid_t type_id = type.getId();
   hid_t space_id = space.getId();
   hid_t xfer_plist_id = xfer_plist.getId();

   herr_t ret_value = H5Dvlen_reclaim(type_id, space_id, xfer_plist_id, buf);
   if (ret_value < 0)
   {
      throw DataSetIException("DataSet::vlenReclaim", "H5Dvlen_reclaim failed");
   }
}

//--------------------------------------------------------------------------
// Function:	DataSet::read
///\brief	Reads raw data from the specified dataset.
///\param	buf - IN: Buffer for read data
///\param	mem_type - IN: Memory datatype
///\param	mem_space - IN: Memory dataspace
///\param	file_space - IN: Dataset's dataspace in the file
///\param	xfer_plist - IN: Transfer property list for this I/O operation
///\exception	H5::DataSetIException
///\par Description
///		This function reads raw data from this dataset into the
///		buffer \a buf, converting from file datatype and dataspace
///		to memory datatype \a mem_type and dataspace \a mem_space.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
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

//--------------------------------------------------------------------------
// Function:	DataSet::read
///\brief	This is an overloaded member function, provided for convenience.
///		It takes a reference to a \c H5std_string for the buffer.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void DataSet::read( H5std_string& strg, const DataType& mem_type, const DataSpace& mem_space, const DataSpace& file_space, const DSetMemXferPropList& xfer_plist ) const
{
   // Allocate C character string for reading
   size_t size = mem_type.getSize();
   char* strg_C = new char[size+1];  // temporary C-string for C API

   // Use the overloaded member to read
   read(strg_C, mem_type, mem_space, file_space, xfer_plist);

   // Get the String and clean up
   strg = strg_C;
   delete []strg_C;
}

//--------------------------------------------------------------------------
// Function:	DataSet::write
///\brief	Writes raw data from an application buffer to a dataset.
///\param	buf - IN: Buffer containing data to be written
///\param	mem_type - IN: Memory datatype
///\param	mem_space - IN: Memory dataspace
///\param	file_space - IN: Dataset's dataspace in the file
///\param	xfer_plist - IN: Transfer property list for this I/O operation
///\exception	H5::DataSetIException
///\par Description
///		This function writes raw data from an application buffer
///		\a buf to a dataset, converting from memory datatype
///		\a mem_type and dataspace \a mem_space to file datatype
///		and dataspace.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
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

//--------------------------------------------------------------------------
// Function:	DataSet::write
///\brief	This is an overloaded member function, provided for convenience.
///		It takes a reference to a \c H5std_string for the buffer.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void DataSet::write( const H5std_string& strg, const DataType& mem_type, const DataSpace& mem_space, const DataSpace& file_space, const DSetMemXferPropList& xfer_plist ) const
{
   // Convert string to C-string
   const char* strg_C;
   strg_C = strg.c_str();  // strg_C refers to the contents of strg as a C-str

   // Use the overloaded member
   write(strg_C, mem_type, mem_space, file_space, xfer_plist);
}

//--------------------------------------------------------------------------
// Function:	DataSet::iterateElems
///\brief	Iterates over all selected elements in a dataspace.
///\param	buf - IN/OUT: Pointer to the buffer in memory containing the
///		elements to iterate over
///\param	type - IN: Datatype for the elements stored in \a buf
///\param	space - IN: Dataspace for \a buf. Also contains the selection
///		to iterate over.
///\param	op - IN: Function pointer to the routine to be called for
///		each element in \a buf iterated over
///\param	op_data - IN/OUT: Pointer to any user-defined data associated
///		with the operation
///\exception	H5::DataSetIException
///\note	This function may not work correctly yet - it's still
///		under development.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
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

//--------------------------------------------------------------------------
// Function:	DataSet::extend
///\brief	Extends a dataset with unlimited dimension.
///\param	size - IN: Array containing the new magnitude of each dimension
///\exception	H5::DataSetIException
///\par Description
///		For more information, please see the Description section in
///		C layer Reference Manual at:
///\par
/// http://hdfgroup.org/HDF5/doc/RM_H5D.html#Dataset-Extend
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void DataSet::extend( const hsize_t* size ) const
{
   herr_t ret_value = H5Dextend( id, size );
   if( ret_value < 0 )  // raise exception when H5Dextend returns a neg value
   {
      throw DataSetIException("DataSet::extend", "H5Dextend failed");
   }
}

//--------------------------------------------------------------------------
// Function:	DataSet::fillMemBuf
///\brief	Fills a selection in memory with a value.
///\param	fill - IN: Pointer to fill value to use - default NULL
///\param	fill_type - IN: Datatype of the fill value
///\param	buf - IN/OUT: Memory buffer to fill selection within
///\param	buf_type - IN: Datatype of the elements in buffer
///\param	space - IN: Dataspace describing memory buffer & containing selection to use
///\exception	H5::DataSetIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void DataSet::fillMemBuf(const void *fill, DataType& fill_type, void *buf, DataType& buf_type, DataSpace& space)
{
    hid_t fill_type_id = fill_type.getId();
    hid_t buf_type_id = buf_type.getId();
    hid_t space_id = space.getId();
    herr_t ret_value = H5Dfill(fill, fill_type_id, buf, buf_type_id, space_id);
    if( ret_value < 0 )
    {
	throw DataSetIException("DataSet::fillMemBuf", "H5Dfill failed");
    }
}

//--------------------------------------------------------------------------
// Function:	DataSet::fillMemBuf
///\brief	This is an overloaded member function, provided for convenience.
///		It differs from the above function in that it only takes the
///		the last three arguments.
///\param	buf - IN/OUT: Memory buffer to fill selection within
///\param	buf_type - IN: Datatype of the elements in buffer
///\param	space - IN: Dataspace describing memory buffer & containing selection to use
///\exception	H5::DataSetIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void DataSet::fillMemBuf(void *buf, DataType& buf_type, DataSpace& space)
{
    hid_t buf_type_id = buf_type.getId();
    hid_t space_id = space.getId();
    herr_t ret_value = H5Dfill(NULL, buf_type_id, buf, buf_type_id, space_id);
    if( ret_value < 0 )
    {
	throw DataSetIException("DataSet::fillMemBuf", "H5Dfill failed");
    }
}

//--------------------------------------------------------------------------
// Function:	DataSet::Reference
///\brief	Important!!! - This functions may not work correctly, it 
///		will be removed in the near future.  Please use 
///		DataSet::reference instead!
// Programmer	Binh-Minh Ribler - May, 2004
//--------------------------------------------------------------------------
void* DataSet::Reference(const char* name, DataSpace& dataspace, H5R_type_t ref_type) const
{
   try {
      return(p_reference(name, dataspace.getId(), ref_type));
   }
   catch (IdComponentException E) {
      throw DataSetIException("DataSet::Reference", E.getDetailMsg());
   }
}

//--------------------------------------------------------------------------
// Function:	DataSet::Reference
///\brief	Important!!! - This functions may not work correctly, it 
///		will be removed in the near future.  Please use similar
///		DataSet::reference instead!
// Programmer	Binh-Minh Ribler - May, 2004
//--------------------------------------------------------------------------
void* DataSet::Reference(const char* name) const
{
   try {
      return(p_reference(name, -1, H5R_OBJECT));
   }
   catch (IdComponentException E) {
      throw DataSetIException("DataSet::Reference", E.getDetailMsg());
   }
}

//--------------------------------------------------------------------------
// Function:	DataSet::Reference
///\brief	Important!!! - This functions may not work correctly, it 
///		will be removed in the near future.  Please use similar
///		DataSet::reference instead!
// Programmer	Binh-Minh Ribler - May, 2004
//--------------------------------------------------------------------------
void* DataSet::Reference(const H5std_string& name) const
{
   return(Reference(name.c_str()));
}

//--------------------------------------------------------------------------
// Function:	DataSet::getObjType
///\brief	Retrieves the type of object that an object reference points to.
///\param	ref      - IN: Reference to query
///\param	ref_type - IN: Type of reference to query, valid values are:
///		\li \c H5R_OBJECT - Reference is an object reference.
///		\li \c H5R_DATASET_REGION - Reference is a dataset region reference.
///\return	An object type, which can be one of the following:
///		\li \c H5G_LINK (0) - Object is a symbolic link.
///		\li \c H5G_GROUP (1) - Object is a group.
///		\li \c H5G_DATASET (2) - Object is a dataset.
///		\li \c H5G_TYPE Object (3) - is a named datatype
///\exception	H5::DataSetIException
// Programmer	Binh-Minh Ribler - May, 2004
//--------------------------------------------------------------------------
H5G_obj_t DataSet::getObjType(void *ref, H5R_type_t ref_type) const
{
   try {
      return(p_get_obj_type(ref, ref_type));
   }
   catch (IdComponentException E) {
      throw DataSetIException("DataSet::getObjType", E.getDetailMsg());
   }
}

//--------------------------------------------------------------------------
// Function:	DataSet::getRegion
///\brief	Retrieves a dataspace with the region pointed to selected.
///\param	ref_type - IN: Type of reference to get region of - default
///		to H5R_DATASET_REGION
///\param	ref      - IN: Reference to get region of
///\return	DataSpace instance
///\exception	H5::DataSetIException
// Programmer	Binh-Minh Ribler - May, 2004
//--------------------------------------------------------------------------
DataSpace DataSet::getRegion(void *ref, H5R_type_t ref_type) const
{
   try {
      DataSpace dataspace(p_get_region(ref, ref_type));
      return(dataspace);
   }
   catch (IdComponentException E) {
      throw DataSetIException("DataSet::getRegion", E.getDetailMsg());
   }
}

//--------------------------------------------------------------------------
// Function:	DataSet::close
///\brief	Closes this dataset.
///
///\exception	H5::DataSetIException
// Programmer	Binh-Minh Ribler - Mar 9, 2005
//--------------------------------------------------------------------------
void DataSet::close()
{
    if (p_valid_id(id))
    {
	herr_t ret_value = H5Dclose( id );
	if( ret_value < 0 )
	{
	    throw DataSetIException("DataSet::close", "H5Dclose failed");
	}
	// reset the id because the dataset that it represents is now closed
	id = 0;
    }
}

//--------------------------------------------------------------------------
// Function:	DataSet destructor
///\brief	Properly terminates access to this dataset.
// Programmer	Binh-Minh Ribler - 2000
// Modification
//		- Replaced resetIdComponent() with decRefCount() to use C
//		library ID reference counting mechanism - BMR, Feb 20, 2005
//		- Replaced decRefCount with close() to let the C library
//		handle the reference counting - BMR, Jun 1, 2006
//--------------------------------------------------------------------------
DataSet::~DataSet()
{
    try {
	close();
    }
    catch (Exception close_error) {
	cerr << "DataSet::~DataSet - " << close_error.getDetailMsg() << endl;
    }
}

#ifndef H5_NO_NAMESPACE
} // end namespace
#endif
