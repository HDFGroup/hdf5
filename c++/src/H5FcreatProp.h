#ifndef _H5FileCreatPropList_H
#define _H5FileCreatPropList_H

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

// class for file access properties
class FileCreatPropList : public PropList {
   public:
	static const FileCreatPropList DEFAULT;
	
	// Creates a file create property list.
	FileCreatPropList();

	// Copy constructor: creates a copy of a FileCreatPropList object
	FileCreatPropList( const FileCreatPropList& orig );

	// Retrieves version information for various parts of a file.
	void getVersion( int& boot, int& freelist, int& stab, int& shhdr ) const;

	// Sets the userblock size field of a file creation property list.
	void setUserblock( hsize_t size ) const;

	// Gets the size of a user block in this file creation property list.
	hsize_t getUserblock() const;

	// Sets file size-of addresses and sizes.
	void setSizes( size_t sizeof_addr = 4, size_t sizeof_size = 4 ) const;

	// Retrieves the size-of address and size quantities stored in a 
	// file according to this file creation property list.
	void getSizes( size_t& sizeof_addr, size_t& sizeof_size ) const;

	// Sets the size of parameters used to control the symbol table nodes.
	void setSymk( int int_nodes_k, int leaf_nodes_k ) const;

	// Retrieves the size of the symbol table B-tree 1/2 rank and the
	// symbol table leaf node 1/2 size.
	void getSymk( int& int_nodes_k, int& leaf_nodes_k ) const;

	// Sets the size of parameter used to control the B-trees for
	// indexing chunked datasets.
	void setIstorek( int ik ) const;

	// Returns the 1/2 rank of an indexed storage B-tree.
	int getIstorek() const;

	// Creates a copy of an existing file create property list
	// using the property list id
	FileCreatPropList (const hid_t plist_id) : PropList( plist_id ) {}

	// Default destructor
	virtual ~FileCreatPropList();
};
#ifndef H5_NO_NAMESPACE
}
#endif
#endif
