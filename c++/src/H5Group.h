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

#ifndef _H5Group_H
#define _H5Group_H

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

class H5_DLLCPP Group : public H5Object, public CommonFG {
   public:
	// Close this group.
	virtual void close();

	// Retrieves the type of object that an object reference points to.
	H5G_obj_t getObjType(void *ref, H5R_type_t ref_type) const;

	// Retrieves a dataspace with the region pointed to selected.
	DataSpace getRegion(void *ref, H5R_type_t ref_type = H5R_DATASET_REGION) const;

	// Creates a reference to a named Hdf5 object or to a dataset region
	// in this object.
	void* Reference(const char* name, DataSpace& dataspace, H5R_type_t ref_type = H5R_DATASET_REGION) const;

	// Creates a reference to a named Hdf5 object in this object.
	void* Reference(const char* name) const;

        // Throw group exception.
        virtual void throwException(const string func_name, const string msg) const;

	// for CommonFG to get the file id.
	virtual hid_t getLocId() const;

	// default constructor
	Group();

	// Copy constructor: makes a copy of the original object
	Group(const Group& original);

	// Destructor
	virtual ~Group();

        // Creates a copy of an existing group using its id.
        Group( const hid_t group_id );

};
#ifndef H5_NO_NAMESPACE
}
#endif
#endif
