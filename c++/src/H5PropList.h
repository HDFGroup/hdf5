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

#ifndef _H5PropList_H
#define _H5PropList_H

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

class H5_DLLCPP PropList : public IdComponent {
   public:
	// Default property list
        static const PropList DEFAULT;

	// Creates a property list of a given type or creates a copy of an
	// existing property list giving the property list id.
	PropList(const hid_t plist_id);

	// Make a copy of the given property list using assignment statement
	PropList& operator=( const PropList& rhs );

	// Compares this property list or class against the given list or class.
	bool operator==(const PropList& rhs) const;

	// Close this property list.
	virtual void close();

	// Close a property list class.
	void closeClass() const;

	// Makes a copy of the given property list.
	void copy( const PropList& like_plist );

	// Copies a property from this property list or class to another
	void copyProp( PropList& dest, const char* name) const;
	void copyProp( PropList& dest, const string& name) const;

	// Copies a property from one property list or property class to another
	void copyProp( PropList& dest, PropList& src, const char* name) const;
	void copyProp( PropList& dest, PropList& src, const string& name) const;

	// Gets the class of this property list, i.e. H5P_FILE_CREATE,
	// H5P_FILE_ACCESS, ...
	hid_t getClass() const;

	// Return the name of a generic property list class.
	string getClassName() const;

	// Returns the parent class of a generic property class.
	PropList getClassParent() const;

	// Returns the number of properties in this property list or class.
	size_t getNumProps() const;

	// Query the value of a property in a property list.
	void getProperty(const char* name, void* value) const;
	void getProperty(const string& name, void* value) const;
	string getProperty(const char* name) const;
	string getProperty(const string& name) const;

	// Set a property's value in a property list.
	void setProperty(const char* name, void* value) const;
	void setProperty(const char* name, const char* charptr) const;
	void setProperty(const char* name, string& strg) const;
	void setProperty(const string& name, void* value) const;
	void setProperty(const string& name, string& strg) const;

	// Query the size of a property in a property list or class.
	size_t getPropSize(const char *name) const;
	size_t getPropSize(const string& name) const;

	// Determines whether a property list is a certain class.
	bool isAClass(const PropList& prop_class) const;

	/// Query the existance of a property in a property object.
	bool propExist(const char* name) const;
	bool propExist(const string& name) const;

	// Removes a property from a property list.
	void removeProp(const char *name) const;
	void removeProp(const string& name) const;

	// Returns this class name
	virtual string fromClass () const { return ("PropList"); }

	// Default constructor: creates a stub PropList object.
	PropList();

	// Copy constructor: creates a copy of a PropList object.
	PropList(const PropList& original);

	// Destructor: properly terminates access to this property list.
	virtual ~PropList();
};

#ifndef H5_NO_NAMESPACE
}
#endif
#endif  // _H5PropList_H
