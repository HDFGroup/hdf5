#ifndef _H5Group_H
#define _H5Group_H

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

class Group : public H5Object {
   public:
	// default constructor
	Group();

	// Copy constructor: makes a copy of the original object
	Group( const Group& original );

	// Creates a group in this group
	Group createGroup( const string& name, size_t size_hint = 0 );
	Group createGroup( const char* name, size_t size_hint = 0 );

	// Opens an existing group in this group 
	Group openGroup( const string& name );
	Group openGroup( const char* name );

	// Creates a dataset in this group
	DataSet createDataSet( const string& name, const DataType& data_type, const DataSpace& data_space, const DSetCreatPropList& create_plist = DSetCreatPropList::DEFAULT );
	DataSet createDataSet( const char* name, const DataType& data_type, const DataSpace& data_space, const DSetCreatPropList& create_plist = DSetCreatPropList::DEFAULT );

	// Opens a dataset in this group
	DataSet openDataSet( const string& name );
	DataSet openDataSet( const char* name );

	// Opens a generic named datatype in this group.
	DataType openDataType( const string& name ) const;
	DataType openDataType( const char* name ) const;

	// Opens a named enumeration datatype in this group.
	EnumType openEnumType( const string& name ) const;
	EnumType openEnumType( const char* name ) const;

	// Opens a named compound datatype in this group.
	CompType openCompType( const string& name ) const;
	CompType openCompType( const char* name ) const;

	// Opens a named integer datatype in this group.
	IntType openIntType( const string& name ) const;
	IntType openIntType( const char* name ) const;

	// Opens a named floating-point datatype in this group.
	FloatType openFloatType( const string& name ) const;
	FloatType openFloatType( const char* name ) const;

	// Opens a named string datatype in this group.
	StrType openStrType( const string& name ) const;
	StrType openStrType( const char* name ) const;

	// Creates a link from new_name to current_name in this group.
	void link( H5G_link_t link_type, const string& curr_name, const string& new_name );
	void link( H5G_link_t link_type, const char* curr_name, const char* new_name );

	// Removes a name linked to this group.
	void unlink( const string& name );
	void unlink( const char* name );

	// Iterates over the elements of this group - not implemented in 
	// C++ style yet
	int iterateElems( const string& name, int *idx, H5G_iterate_t op, void *op_data );
	int iterateElems( const char* name, int *idx, H5G_iterate_t op, void *op_data );

	// Renames an object within this group.
	void move( const string& src, const string& dst );
	void move( const char* src, const char* dst );

	// Retrieves information about the named object.
	void getObjinfo( const string& name, hbool_t follow_link, H5G_stat_t& statbuf );
	void getObjinfo( const char* name, hbool_t follow_link, H5G_stat_t& statbuf );

	// Returns the name of the object that the symbolic link points to.
	string getLinkval( const string& name, size_t size );
	string getLinkval( const char* name, size_t size );

	// Sets comment for an object specified by its name.
	void setComment( const string& name, const string& comment );
	void setComment( const char* name, const char* comment );

	// Gets the comment of an object specified by its name.
	string getComment( const string& name, size_t bufsize );
	string getComment( const char* name, size_t bufsize );

	// Mounts the file 'child' onto this group.
	void mount( const string& name, H5File& child, PropList& plist);
	void mount( const char* name, H5File& child, PropList& plist);

	// Unmounts the file named 'name' from this parent group.
	void unmount( const string& name );
	void unmount( const char* name );

	// Used by the API to appropriately close a group
	void p_close() const;

	virtual ~Group();

        // Creates a copy of an existing Group using its id
        // (used only by template functions in FGtemplates.h
	// to return a Group; will not be published; maybe, use friend???)
        Group( const hid_t group_id );

   private:
        // Common code for member functions openXxxType - templates, maybe???
        hid_t p_openDataType( const char* name ) const;

};
#ifndef H5_NO_NAMESPACE
}
#endif
#endif
