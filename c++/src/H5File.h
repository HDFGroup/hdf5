#ifndef _H5File_H
#define _H5File_H

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

class H5File : public IdComponent, public CommonFG {
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

	// Gets the file id
	virtual hid_t getLocId() const;

	// Throw file exception
	virtual void throwException() const;

	// Determines if a file, specified by its name, is in HDF5 format
	static bool isHdf5(const string& name );
	static bool isHdf5(const char* name );

	// Reopens this file
	void reopen();

	// Gets the creation property list of this file
	FileCreatPropList getCreatePlist() const;

	// Gets the access property list of this file
	FileAccPropList getAccessPlist() const;

	// Used by the API to appropriately close a file
	void p_close() const;

	virtual ~H5File();

   private:
	// This function is private and contains common code between the
	// constructors taking a string or a char*
	void getFile( const char* name, unsigned int flags, const FileCreatPropList& create_plist, const FileAccPropList& access_plist );

};
#ifndef H5_NO_NAMESPACE
}
#endif
#endif
