#ifndef _H5DSCreatPropList_H
#define _H5DSCreatPropList_H

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

class DSetCreatPropList : public PropList {
   public:
	static const DSetCreatPropList DEFAULT;

	// Creates a dataset creation property list
	DSetCreatPropList() : PropList( H5P_DATASET_CREATE ) {}

	// Copy constructor: creates a copy of a DSetCreatPropList object;
	// often used by the compiler when passing by value occurs.
	DSetCreatPropList( const DSetCreatPropList& orig );

	// Sets the type of storage used to store the raw data for the 
	// dataset that uses this property list
	void setLayout(hid_t plist, H5D_layout_t layout ) const;

	// Gets the layout of the raw data storage of the data that uses this
	// property list
	H5D_layout_t getLayout() const;

	// Sets the size of the chunks used to store a chunked layout dataset.
	void setChunk( int ndims, const hsize_t* dim ) const;

	// Retrieves the size of the chunks used to store a chunked layout dataset.
	int getChunk( int max_ndims, hsize_t* dim ) const;

	// Sets compression method and compression level
	void setDeflate( int level ) const;

	// Sets a dataset fill value
	void setFillValue( DataType& fvalue_type, const void* value ) const;

	// Retrieves a dataset fill value
	void getFillValue( DataType& fvalue_type, void* value ) const;

	// Adds a filter to the filter pipeline
	void setFilter( H5Z_filter_t filter, unsigned int flags, size_t cd_nelmts, const unsigned int cd_values[] ) const;

	// Returns the number of filters in the pipeline 
	int getNfilters() const;

	// Returns information about a filter in a pipeline
	H5Z_filter_t getFilter( int filter_number, unsigned int& flags, size_t& cd_nelmts, unsigned int* cd_values, size_t namelen, char name[] ) const;

	// Adds an external file to the list of external files
	void setExternal( const char* name, off_t offset, hsize_t size ) const;

	// Returns the number of external files for a dataset 
	int getExternalCount() const;

	// Returns information about an external file
	void getExternal( int idx, size_t name_size, char* name, off_t& offset, hsize_t& size ) const;

	// Creates a copy of an existing dataset creation property list 
	// using the property list id
	DSetCreatPropList( const hid_t plist_id ) : PropList( plist_id ) {}

	// Default destructor
	virtual ~DSetCreatPropList();
};
#ifndef H5_NO_NAMESPACE
}
#endif
#endif
