#ifndef _H5EnumType_H
#define _H5EnumType_H

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

class EnumType : public DataType {

   public:
	// Creates an empty enumeration datatype based on a native signed 
	// integer type, whose size is given by size.
	EnumType( size_t size );

	// Default constructor
	EnumType();

	// Creates an enumeration datatype using an existing id
	EnumType( const hid_t existing_id );

	// Copy constructor: makes a copy of the original EnumType object.
	EnumType( const EnumType& original );

	// Gets the enum datatype of the specified dataset
	EnumType( const DataSet& dataset );  // H5Dget_type

	// Creates a new enum datatype based on an integer datatype
	EnumType( const IntType& data_type );  // H5Tenum_create

	// Inserts a new member to this enumeration type. 
	void insert( const string& name, void *value ) const;
	void insert( const char* name, void *value ) const;

	// Returns the symbol name corresponding to a specified member 
	// of this enumeration datatype. 
	string nameOf( void *value, size_t size ) const;

	// Returns the value corresponding to a specified member of this 
	// enumeration datatype. 
	void valueOf( const string& name, void *value ) const;
	void valueOf( const char* name, void *value ) const;

	// Returns the value of an enumeration datatype member
	void getMemberValue( int memb_no, void *value ) const;

	virtual ~EnumType();
};
#ifndef H5_NO_NAMESPACE
}
#endif
#endif
