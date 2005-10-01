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

// CommonFG is a protocol class.  Its existence is simply to provide the
// common services that are provided by H5File and Group.  The file or
// group in the context of this class is referred to as 'location'.

#ifndef _CommonFG_H
#define _CommonFG_H

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

class Group;
class H5File;
class ArrayType;
class VarLenType;
class H5_DLLCPP CommonFG {
   public:
	// Creates a new group at this location which can be a file
	// or another group.
	Group createGroup(const char* name, size_t size_hint = 0) const;
	Group createGroup(const string& name, size_t size_hint = 0) const;

	// Opens an existing group in a location which can be a file
	// or another group.
	Group openGroup(const char* name) const;
	Group openGroup(const string& name) const;

	// Creates a new dataset at this location.
	DataSet createDataSet(const char* name, const DataType& data_type, const DataSpace& data_space, const DSetCreatPropList& create_plist = DSetCreatPropList::DEFAULT) const;
	DataSet createDataSet(const string& name, const DataType& data_type, const DataSpace& data_space, const DSetCreatPropList& create_plist = DSetCreatPropList::DEFAULT) const;

	// Opens an existing dataset at this location.
	DataSet openDataSet(const char* name) const;
	DataSet openDataSet(const string& name) const;

	// Retrieves comment for the HDF5 object specified by its name.
	string getComment(const string& name) const;
	string getComment(const char* name, size_t bufsize) const;
	string getComment(const string& name, size_t bufsize) const;

	// Removes the comment for the HDF5 object specified by its name.
	void removeComment(const char* name) const;
	void removeComment(const string& name) const;

	// Sets the comment for an HDF5 object specified by its name.
	void setComment(const char* name, const char* comment) const;
	void setComment(const string& name, const string& comment) const;

	// Returns the name of the HDF5 object that the symbolic link points to.
	string getLinkval(const char* name, size_t size) const;
	string getLinkval(const string& name, size_t size) const;

	// Returns the number of objects in this group.
	hsize_t getNumObjs() const;

	// Returns information about an HDF5 object, given by its name,
	// at this location.
	void getObjinfo(const char* name, hbool_t follow_link, H5G_stat_t& statbuf) const;
	void getObjinfo(const string& name, hbool_t follow_link, H5G_stat_t& statbuf) const;

	// Retrieves the name of an object in this group, given the
	// object's index.
	ssize_t getObjnameByIdx(hsize_t idx, string& name, size_t size) const;
	string getObjnameByIdx(hsize_t idx) const;

	// Returns the type of an object in this group, given the
	// object's index.
	H5G_obj_t getObjTypeByIdx(hsize_t idx) const;
	H5G_obj_t getObjTypeByIdx(hsize_t idx, string& type_name) const;

	// Iterates over the elements of this group - not implemented in
	// C++ style yet.
	int iterateElems(const char* name, int *idx, H5G_iterate_t op, void *op_data);
	int iterateElems(const string& name, int *idx, H5G_iterate_t op, void *op_data);

	// Creates a link of the specified type from new_name to current_name;
	// both names are interpreted relative to the specified location id.
	void link(H5G_link_t link_type, const char* curr_name, const char* new_name) const;
	void link(H5G_link_t link_type, const string& curr_name, const string& new_name) const;

	// Removes the specified name at this location.
	void unlink(const char* name) const;
	void unlink(const string& name) const;

	// Mounts the file 'child' onto this location.
	void mount(const char* name, H5File& child, PropList& plist) const;
	void mount(const string& name, H5File& child, PropList& plist) const;

	// Unmounts the file named 'name' from this parent location.
	void unmount(const char* name) const;
	void unmount(const string& name) const;

	// Renames an object at this location.
	void move(const char* src, const char* dst) const;
	void move(const string& src, const string& dst) const;

	// Opens a generic named datatype in this location.
	DataType openDataType(const char* name) const;
	DataType openDataType(const string& name) const;

	// Opens a named array datatype in this location.
	ArrayType openArrayType(const char* name) const;
	ArrayType openArrayType(const string& name) const;

	// Opens a named compound datatype in this location.
	CompType openCompType(const char* name) const;
	CompType openCompType(const string& name) const;

	// Opens a named enumeration datatype in this location.
	EnumType openEnumType(const char* name) const;
	EnumType openEnumType(const string& name) const;

	// Opens a named integer datatype in this location.
	IntType openIntType(const char* name) const;
	IntType openIntType(const string& name) const;

	// Opens a named floating-point datatype in this location.
	FloatType openFloatType(const char* name) const;
	FloatType openFloatType(const string& name) const;

	// Opens a named string datatype in this location.
	StrType openStrType(const char* name) const;
	StrType openStrType(const string& name) const;

	// Opens a named variable length datatype in this location.
	VarLenType openVarLenType(const char* name) const;
	VarLenType openVarLenType(const string& name) const;

	/// For subclasses, H5File and Group, to return the correct
	/// object id, i.e. file or group id.
	virtual hid_t getLocId() const = 0;

	/// For subclasses, H5File and Group, to throw appropriate exception.
	virtual void throwException(const string func_name, const string msg) const = 0;

	// Default constructor.
	CommonFG();

	// Noop destructor.
	virtual ~CommonFG();

   private:
	// Common code for member functions openXxxType
	hid_t p_open_data_type(const char* name) const;

}; // end of CommonFG declaration

#ifndef H5_NO_NAMESPACE
}
#endif
#endif

