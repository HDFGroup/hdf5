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

#ifndef _H5DSetMemXferPropList_H
#define _H5DSetMemXferPropList_H

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

class __DLLCPP__ DSetMemXferPropList : public PropList {
   public:
	static const DSetMemXferPropList DEFAULT;

	// Creates a dataset memory and transfer property list
	DSetMemXferPropList();

	// Copy constructor: creates a copy of a DSetMemXferPropList object
	DSetMemXferPropList( const DSetMemXferPropList& orig );

	// Sets type conversion and background buffers
	void setBuffer( size_t size, void* tconv, void* bkg ) const;

	// Reads buffer settings
	size_t getBuffer( void** tconv, void** bkg ) const;

	// Sets the dataset transfer property list status to TRUE or FALSE
	void setPreserve( bool status ) const;

	// Checks status of the dataset transfer property list
	bool getPreserve() const;

	// Indicates whether to cache hyperslab blocks during I/O
	void setHyperCache( bool cache, unsigned limit = 0 ) const;

	// Returns information regarding the caching of hyperslab blocks during I/O
	void getHyperCache( bool& cache, unsigned& limit ) const;

	// Sets B-tree split ratios for a dataset transfer property list 
	void setBtreeRatios( double left, double middle, double right ) const;

	// Gets B-tree split ratios for a dataset transfer property list
	void getBtreeRatios( double& left, double& middle, double& right ) const;

	// Sets the memory manager for variable-length datatype 
	// allocation in H5Dread and H5Dvlen_reclaim
	void setVlenMemManager( H5MM_allocate_t alloc, void* alloc_info, 
				H5MM_free_t free, void* free_info ) const;

	// alloc and free are set to NULL, indicating that system 
	// malloc and free are to be used
	void setVlenMemManager() const;

	// Gets the memory manager for variable-length datatype 
	// allocation in H5Dread and H5Tvlen_reclaim
	void getVlenMemManager( H5MM_allocate_t& alloc, void** alloc_info, 
				H5MM_free_t& free, void** free_info ) const;

	// Sets the transfer mode - parallel mode, not currently supported
	//void setXfer( H5D_transfer_t data_xfer_mode = H5D_XFER_INDEPENDENT ) const;

	// Gets the transfer mode - parallel mode, not currently supported
	//H5D_transfer_t getXfer() const;

	// Creates a copy of an existing dataset memory and transfer 
	// property list using the property list id
	DSetMemXferPropList (const hid_t plist_id) : PropList( plist_id ) {}

	// Default destructor
	virtual ~DSetMemXferPropList();

};
#ifndef H5_NO_NAMESPACE
}
#endif
#endif
