/*
This class is a prototype class.  Most of its member functions are those
that are common to both H5File and Group.  H5File and Group will inherit
these functions.
*/

#ifndef _CommonFG_H
#define _CommonFG_H

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

class Group;
class H5File;
class CommonFG {
   public:
	// Creates a new group at this location which can be a file or another group.
	Group createGroup( const string& name, size_t size_hint = 0 ) const;
	Group createGroup( const char* name, size_t size_hint = 0 ) const;

	// Opens an existing group in a location which can be a file or another group
	Group openGroup( const string& name ) const;
	Group openGroup( const char* name ) const;

	// Creates a new dataset at this location.
	DataSet createDataSet( const string& name, const DataType& data_type, const DataSpace& data_space, const DSetCreatPropList& create_plist = DSetCreatPropList::DEFAULT ) const;
	DataSet createDataSet( const char* name, const DataType& data_type, const DataSpace& data_space, const DSetCreatPropList& create_plist = DSetCreatPropList::DEFAULT ) const;

	// Opens an existing dataset at this location.
	DataSet openDataSet( const string& name ) const;
	DataSet openDataSet( const char* name ) const;

	// Creates a link of the specified type from new_name to current_name;
	// both names are interpreted relative to the specified location id
	void link( H5G_link_t link_type, const string& curr_name, const string& new_name ) const;
	void link( H5G_link_t link_type, const char* curr_name, const char* new_name ) const;

	// Removes the specified name at this location.
	void unlink( const string& name ) const;
	void unlink( const char* name ) const;

	// Get id of the location, either group or file - pure virtual so
	// the subclass can get the correct id
	virtual hid_t getLocId() const = 0; 

	// Renames an object at this location.
	void move( const string& src, const string& dst ) const;
	void move( const char* src, const char* dst ) const;

	// Returns information about an object
	void getObjinfo( const string& name, hbool_t follow_link, H5G_stat_t& statbuf ) const;
	void getObjinfo( const char* name, hbool_t follow_link, H5G_stat_t& statbuf ) const;

	// Returns the name of the object that the symbolic link points to.
	string getLinkval( const string& name, size_t size ) const;
	string getLinkval( const char* name, size_t size ) const;

	// Sets the comment for an object specified by its name
	void setComment( const string& name, const string& comment ) const;
	void setComment( const char* name, const char* comment ) const;

	// Retrieves comment for specified object
	string getComment( const string& name, size_t bufsize ) const;
	string getComment( const char* name, size_t bufsize ) const;

	// Mounts the file 'child' onto this group
	void mount( const string& name, H5File& child, PropList& plist ) const;
	void mount( const char* name, H5File& child, PropList& plist) const;

	// Unmounts the file named 'name' from this parent group
	void unmount( const string& name ) const;
	void unmount( const char* name ) const;

	// Iterates over the elements of this group - not implemented in
	// C++ style yet
	int iterateElems( const string& name, int *idx, H5G_iterate_t op, void *op_data );
	int iterateElems( const char* name, int *idx, H5G_iterate_t op, void *op_data );

	// Opens a generic named datatype in this file
	DataType openDataType( const string& name ) const;
	DataType openDataType( const char* name ) const;

	// Opens a named enumeration datatype in this file
	EnumType openEnumType( const string& name ) const;
	EnumType openEnumType( const char* name ) const;

	// Opens a named compound datatype in this file
	CompType openCompType( const string& name ) const;
	CompType openCompType( const char* name ) const;

	// Opens a named integer datatype in this file
	IntType openIntType( const string& name ) const;
	IntType openIntType( const char* name ) const;

	// Opens a named floating-point datatype in this file
	FloatType openFloatType( const string& name ) const;
	FloatType openFloatType( const char* name ) const;

	// Opens a named string datatype in this file
	StrType openStrType( const string& name ) const;
	StrType openStrType( const char* name ) const;

	// for H5File and Group to throw appropriate exception
	virtual void throwException() const = 0;

	CommonFG();

	virtual ~CommonFG();

   private:
	// Common code for member functions openXxxType 
	hid_t p_openDataType( const char* name ) const;

}; // end of CommonFG declaration

#ifndef H5_NO_NAMESPACE
}
#endif
#endif

