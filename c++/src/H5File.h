#ifndef _H5File_H
#define _H5File_H

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

class H5File : public IdComponent {
   public:
	// copy constructor: makes a copy of the original H5File object.
	H5File(const H5File& original );

	// Creates or opens an HDF5 file.
	H5File( const string& name, unsigned int flags,
	   const FileCreatPropList& create_plist = FileCreatPropList::DEFAULT,
	   const FileAccPropList& access_plist = FileAccPropList::DEFAULT );
	H5File( const char* name, unsigned int flags,
	   const FileCreatPropList& create_plist = FileCreatPropList::DEFAULT,
	   const FileAccPropList& access_plist = FileAccPropList::DEFAULT );

	// Sets and gets H5File's data member
	//void setId( hid_t new_file_id );
	//hid_t getId() const;

	// Creates a new group in this file
	Group createGroup( const string& name, size_t size_hint = 0 ) const;
	Group createGroup( const char* name, size_t size_hint = 0 ) const;

	// Opens an existing group in this file
	Group openGroup( const string& name ) const;
	Group openGroup( const char* name ) const;

	// Determines if a file, specified by its name, is in HDF5 format
	static bool isHdf5(const string& name );
	static bool isHdf5(const char* name );

	// Reopens this file
	void reopen();

	// Creates a new dataset in this file
	DataSet createDataSet( const string& name, const DataType& data_type, const DataSpace& data_space, const DSetCreatPropList& create_plist = DSetCreatPropList::DEFAULT ) const;
	DataSet createDataSet( const char* name, const DataType& data_type, const DataSpace& data_space, const DSetCreatPropList& create_plist = DSetCreatPropList::DEFAULT ) const;

	// Opens a existing dataset in this file
	DataSet openDataSet( const string& name ) const;
	DataSet openDataSet( const char* name ) const;

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

	// Gets the creation property list of this file
	FileCreatPropList getCreatePlist() const;

	// Gets the access property list of this file
	FileAccPropList getAccessPlist() const;

	// Creates a link from new_name to current_name in this file
	void link( H5G_link_t link_type, const string& curr_name, const string& new_name ) const;
	void link( H5G_link_t link_type, const char* curr_name, const char* new_name ) const;

	// Removes a name linked to this file
	void unlink( const string& name ) const;
	void unlink( const char* name ) const;

	// Renames an object within this file
	void move( const string& src, const string& dst ) const;
	void move( const char* src, const char* dst ) const;

	// Retrieves information about an object given its name and link
	void getObjinfo( const string& name, hbool_t follow_link, H5G_stat_t& statbuf ) const;
	void getObjinfo( const char* name, hbool_t follow_link, H5G_stat_t& statbuf ) const;

	// Returns the name of the object that the symbolic link 'name'
	// points to
	string getLinkval( const string& name, size_t size ) const;
	string getLinkval( const char* name, size_t size ) const;

	// Sets the comment for an object specified by its name
	void setComment( const string& name, const string& comment ) const;
	void setComment( const char* name, const char* comment ) const;

	// Gets the comment of an object specified by its name
	string getComment( const string& name, size_t bufsize ) const;
	string getComment( const char* name, size_t bufsize ) const;

	// Mounts a file, specified by its name, to this file
	void mount( const string& name, H5File& child, PropList& plist ) const;
	void mount( const char* name, H5File& child, PropList& plist ) const;

	// Unmounts a file, specified by its name, from this file
	void unmount( const string& name ) const;
	void unmount( const char* name ) const;

	// Used by the API to appropriately close a file
	void p_close() const;

	virtual ~H5File();

   private:
	// Common code for member functions openXxxType - templates, maybe???
	hid_t p_openDataType( const char* name ) const;

	// This function is private and contains common code between the
	// constructors taking a string or a char*
	void getFile( const char* name, unsigned int flags, const FileCreatPropList& create_plist, const FileAccPropList& access_plist );

};
#ifndef H5_NO_NAMESPACE
}
#endif
#endif
