#ifndef _H5Attribute_H
#define _H5Attribute_H

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

class Attribute : public AbstractDs {
   public:
	// Writes data to this attribute.
	void write(const DataType& mem_type, void *buf ) const;

	// Reads data from this attribute.
	void read( const DataType& mem_type, void *buf ) const;

	// Gets a copy of the dataspace for this attribute.
	virtual DataSpace getSpace() const;

	// Gets the name of this attribute.
	string getName( size_t buf_size ) const;

	// do not inherit iterateAttrs from H5Object
	int iterateAttrs() { return 0; }

	// Used by the API to appropriately close a attribute
	virtual void p_close() const;

        // Creates a copy of an existing attribute using the attribute id
        Attribute( const hid_t attr_id );

	// Copy constructor: makes a copy of an existing Attribute object.
	Attribute( const Attribute& original );

	virtual ~Attribute();

   private:
	// This function contains the common code that is used by 
	// getTypeClass and various API functions getXxxType 
	// defined in AbstractDs for generic datatype and specific 
	// sub-types
	virtual hid_t p_getType() const;
};
#ifndef H5_NO_NAMESPACE
}
#endif
#endif
