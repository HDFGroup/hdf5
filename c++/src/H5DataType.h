#ifndef _H5DataType_H
#define _H5DataType_H

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

class DataType : public H5Object {
   public:
	// Creates a datatype given its class and size
	DataType( const H5T_class_t type_class, size_t size );

	// Copy constructor: makes a copy of the original object
	DataType( const DataType& original );

	// Creates a new variable-length datatype - not implemented yet
	// Will be moved into a subclass when completed
	//DataType vlenCreate( const DataType& base_type );

	// Copies an existing datatype to this datatype object
	void copy( const DataType& like_type );

	// Returns the datatype class identifier. 
	H5T_class_t getClass() const;

	// Commits a transient datatype to a file; this datatype becomes 
	// a named datatype which can be accessed from the location.
	void commit( H5Object& loc, const string& name ) const;
	void commit( H5Object& loc, const char* name ) const;

	// Determines whether this datatype is a named datatype or 
	// a transient datatype. 
	bool committed() const;

        // Finds a conversion function that can handle the conversion 
        // this datatype to the given datatype, dest.
	H5T_conv_t find( const DataType& dest, H5T_cdata_t **pcdata ) const;

	// Converts data from between specified datatypes. 
	void convert( const DataType& dest, size_t nelmts, void *buf, void *background, PropList& plist ) const;

	// Sets the overflow handler to a specified function. 
	void setOverflow(H5T_overflow_t func) const;

	// Returns a pointer to the current global overflow function. 
	H5T_overflow_t getOverflow(void) const;

	// Determines whether two datatypes are the same. ???
	bool operator==(const DataType& compared_type ) const;

	// Locks a datatype. 
	void lock() const;

	// Returns the size of a datatype. 
	size_t getSize() const;

	// Returns the base datatype from which a datatype is derived. 
	// Note: not quite right for specific types yet???
	DataType getSuper() const;

	// Registers a conversion function. 
	void registerFunc(H5T_pers_t pers, const string& name, const DataType& dest, H5T_conv_t func ) const;
	void registerFunc(H5T_pers_t pers, const char* name, const DataType& dest, H5T_conv_t func ) const;

	// Removes a conversion function from all conversion paths. 
	void unregister( H5T_pers_t pers, const string& name, const DataType& dest, H5T_conv_t func ) const;
	void unregister( H5T_pers_t pers, const char* name, const DataType& dest, H5T_conv_t func ) const;

	// Tags an opaque datatype. 
	void setTag( const string& tag ) const;
	void setTag( const char* tag ) const;

	// Gets the tag associated with an opaque datatype. 
	string getTag() const;

	// Used by the API to appropriately close a datatype
        void p_close() const;

	// Creates a copy of an existing DataType using its id 
	DataType( const hid_t type_id, bool predtype = false );

	// Default constructor
	DataType();

	virtual ~DataType();

   protected:
	bool is_predtype;	// indicates a type is predefined so
				// H5Tclose will not be called for it
};
#ifndef H5_NO_NAMESPACE
}
#endif
#endif
