#ifndef _H5FileAccPropList_H
#define _H5FileAccPropList_H

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

// class for file access properties
class FileAccPropList : public PropList {
   public:
	static const FileAccPropList DEFAULT;

	// Creates a file access property list.
	FileAccPropList();

	// Copy constructor: creates a copy of a FileAccPropList object
	FileAccPropList( const FileAccPropList& orig );

	// Sets the low level file driver to use the functions 
	// declared in the stdio.h
	// void setStdio() const;

	// Determines whether the file access property list is set to the 
	// stdio driver.
	// bool getStdio() const;

	// Sets alignment properties of this file access property list
	void setAlignment( hsize_t threshold = 1, hsize_t alignment = 1 ) const;

	// Retrieves the current settings for alignment properties from
	// this file access property list.
	void getAlignment( hsize_t& threshold, hsize_t& alignment ) const;

	/* MPI stuff not working in serial mode
	//void setMpi( MPI_Comm comm, MPI_Info info ) const;
	//void getMpi( MPI_Comm& comm, MPI_Info& info ) const;
	*/

	// Returns a low-level file driver identifier.
	// H5F_driver_t getDriver() const;

	// Sets the low-level file driver to use the declared functions.
	// void setSec2() const;

	// Determines whether this file access property list is set to the 
	// sec2 driver.
	// bool getSec2() const;

	// Sets the low-level file driver to use malloc() and free().
	// void setCore( size_t increment ) const;

	// Determines whether this file access property list is set to the 
	// core driver and retrieves the increment.
	// bool getCore( size_t& increment ) const;

	// Sets this file access properties list to the family driver.
	// void setFamily( hsize_t memb_size, const FileAccPropList& memb_plist ) const;

	// Determines whether this file access property list is set to the 
	// family driver and retrieves the member's file access property list.
	// bool getFamily( hsize_t& memb_size, FileAccPropList& memb_plist ) const;

	// Sets the meta data cache and raw data chunk cache parameters.
	void setCache( int mdc_nelmts, int rdcc_nelmts, size_t rdcc_nbytes, double rdcc_w0 ) const;

	// Retrieves maximum sizes of data caches and the preemption 
	// policy value.
	void getCache( int& mdc_nelmts, int& rdcc_nelmts, size_t& rdcc_nbytes, double& rdcc_w0 ) const;

	// Sets the low-level driver to split meta data from raw data.
	// void setSplit( FileAccPropList& meta_plist, FileAccPropList& raw_plist, 
	     // const char* meta_ext = ".meta", const char* raw_ext = ".raw" ) const;

	// void setSplit( FileAccPropList& meta_plist, FileAccPropList& raw_plist, 
	     // const string& meta_ext, const string& raw_ext ) const;

	// Determines whether this file access property list is set to the 
	// split driver and retrieves the meta-data and raw-data property lists.
	// void getSplit( size_t meta_ext_size, string& meta_ext, FileAccPropList& 
	     // meta_plist, size_t raw_ext_size, string& raw_ext, FileAccPropList& 
	     // raw_plist ) const;

	// Proposal: 2 separate functions
	//FileAccPropList getMetaPlist( size_t meta_ext_size, char* meta_ext );
	//FileAccPropList getRawPlist( size_t raw_ext_size, char* raw_ext );

	// Sets garbage collecting references flag.
	void setGcReferences( unsigned gc_ref = 0 ) const;

	// Returns garbage collecting references setting.
	unsigned getGcReferences() const;

	// Creates a copy of an existing file access property list
	// using the property list id
	FileAccPropList (const hid_t plist_id) : PropList( plist_id ) {}

	// Default destructor
	virtual ~FileAccPropList();
};
#ifndef H5_NO_NAMESPACE
}
#endif
#endif
