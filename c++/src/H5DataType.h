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

#ifndef _H5DataType_H
#define _H5DataType_H

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

class H5_DLLCPP DataType : public H5Object {
   public:
	// Creates a datatype given its class and size
	DataType( const H5T_class_t type_class, size_t size );

	// Copy constructor: makes a copy of the original object
	DataType( const DataType& original );

	// Closes this datatype.
	virtual void close();

	// Copies an existing datatype to this datatype object
	void copy( const DataType& like_type );

	// Returns the datatype class identifier. 
	H5T_class_t getClass() const;

	// Commits a transient datatype to a file; this datatype becomes 
	// a named datatype which can be accessed from the location.
	void commit( CommonFG& loc, const string& name ) const;
	void commit( CommonFG& loc, const char* name ) const;

	// Determines whether this datatype is a named datatype or 
	// a transient datatype. 
	bool committed() const;

        // Finds a conversion function that can handle the conversion 
        // this datatype to the given datatype, dest.
	H5T_conv_t find( const DataType& dest, H5T_cdata_t **pcdata ) const;

	// Converts data from between specified datatypes. 
	void convert( const DataType& dest, size_t nelmts, void *buf, void *background, PropList& plist ) const;

	// Assignment operator
	DataType& operator=( const DataType& rhs );

	// Determines whether two datatypes are the same.
	bool operator==(const DataType& compared_type ) const;

	// Locks a datatype. 
	void lock() const;

	// Returns the size of a datatype. 
	size_t getSize() const;

	// Returns the base datatype from which a datatype is derived. 
	// Note: not quite right for specific types yet???
	DataType getSuper() const;

	// Registers a conversion function. 
	void registerFunc(H5T_pers_t pers, const string& name, const DataType& dest, H5T_conv_t func ) const;
	void registerFunc(H5T_pers_t pers, const char* name, const DataType& dest, H5T_conv_t func ) const;

	// Removes a conversion function from all conversion paths. 
	void unregister( H5T_pers_t pers, const string& name, const DataType& dest, H5T_conv_t func ) const;
	void unregister( H5T_pers_t pers, const char* name, const DataType& dest, H5T_conv_t func ) const;

	// Tags an opaque datatype. 
	void setTag( const string& tag ) const;
	void setTag( const char* tag ) const;

	// Gets the tag associated with an opaque datatype. 
	string getTag() const;

	// Checks whether this datatype contains (or is) a certain type class.
	bool detectClass(H5T_class_t cls) const;

	// Checks whether this datatype is a variable-length string.
	bool isVariableStr() const;

	// Creates a reference to a named Hdf5 object in this object.
	void* Reference(const char* name) const;

	// Creates a reference to a named Hdf5 object or to a dataset region
	// in this object.
	void* Reference(const char* name, DataSpace& dataspace, H5R_type_t ref_type = H5R_DATASET_REGION) const;

	// Retrieves the type of object that an object reference points to.
	H5G_obj_t getObjType(void *ref, H5R_type_t ref_type) const;

	// Retrieves a dataspace with the region pointed to selected.
	DataSpace getRegion(void *ref, H5R_type_t ref_type = H5R_DATASET_REGION) const;

	// Creates a copy of an existing DataType using its id 
	DataType( const hid_t type_id, bool predtype = false );

	// Default constructor
	DataType();

	// Destructor: properly terminates access to this datatype.
	virtual ~DataType();

   protected:
	bool is_predtype;	// indicates a type is predefined so
				// H5Tclose will not be called for it
};
#ifndef H5_NO_NAMESPACE
}
#endif
#endif
