// C++ informative line for the emacs editor: -*- C++ -*-
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

// Class DataSet inherits from AbstractDs and provides accesses to a dataset.

#ifndef _H5DataSet_H
#define _H5DataSet_H

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

class __DLLCPP__ DataSet : public AbstractDs {
   public:
	// Gets the dataspace of this dataset.
	virtual DataSpace getSpace() const;

	// Gets the creation property list of this dataset.
	DSetCreatPropList getCreatePlist() const;

	// Gets the storage size of this dataset.
	hsize_t getStorageSize() const;

	// - C version not yet implemented??
	hsize_t getVlenBufSize( DataType& type, DataSpace& space ) const;
	void vlenReclaim( DataType& type, DataSpace& space, DSetMemXferPropList& xfer_plist, void* buf ) const;

	// Reads the data of this dataset and stores it in the provided buffer.
	// The memory and file dataspaces and the transferring property list
	// can be defaults.
	void read( void* buf, const DataType& mem_type, const DataSpace& mem_space = DataSpace::ALL, const DataSpace& file_space = DataSpace::ALL, const DSetMemXferPropList& xfer_plist = DSetMemXferPropList::DEFAULT ) const;

	// Writes the buffered data to this dataset.
	// The memory and file dataspaces and the transferring property list
	// can be defaults.
	void write( const void* buf, const DataType& mem_type, const DataSpace& mem_space = DataSpace::ALL, const DataSpace& file_space = DataSpace::ALL, const DSetMemXferPropList& xfer_plist = DSetMemXferPropList::DEFAULT ) const;

	// Iterates the selected elements in the specified dataspace - not implemented in C++ style yet
        int iterateElems( void* buf, const DataType& type, const DataSpace& space, H5D_operator_t op, void* op_data = NULL );

	// Extends the dataset with unlimited dimension.
	void extend( const hsize_t* size ) const;

	// Creates a copy of an existing DataSet using its id
	// (used only by template functions in FGtemplates.h
	// to return a DataSet, will not be published; Note: should use
	// friend template function)
	DataSet( const hid_t dataset_id );

	// Used by the API to appropriately close a dataset
	virtual void p_close() const;

	// Default constructor
	DataSet();

	// Copy constructor
	DataSet( const DataSet& original );

	virtual ~DataSet();

   private:
        // This function contains the common code that is used by
        // getTypeClass and various API functions getXxxType 
        // defined in AbstractDs for generic datatype and specific
        // sub-types
	virtual hid_t p_getType() const;
};
#ifndef H5_NO_NAMESPACE
}
#endif
#endif
