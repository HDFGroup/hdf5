// Class CompType inherits from DataType and provides accesses to a compound
// datatype.

#ifndef _H5CompType_H
#define _H5CompType_H

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

class CompType : public DataType {
   public:
	// Creates a new compound datatype, given the type's size
	CompType( size_t size ); // H5Tcreate

	// Gets the compound datatype of the specified dataset
	CompType( const DataSet& dataset );  // H5Dget_type

	// Returns the number of members in this compound datatype. 
	int getNmembers() const;

	// Returns the name of a member of this compound datatype. 
	string getMemberName( int member_num ) const;

	// Returns the offset of a member of this compound datatype. 
	size_t getMemberOffset( int memb_no ) const;

	// Returns the dimensionality of the specified member. 
	int getMemberDims( int member_num, size_t* dims, int* perm ) const;

	// Returns the type class of the specified member of this compound
	// datatype.  It provides to the user a way of knowing what type
	// to create another datatype of the same class
	H5T_class_t getMemberClass( int member_num ) const;

	// Returns the generic datatype of the specified member in 
	// this compound datatype.
	DataType getMemberDataType( int member_num ) const;

	// Returns the enumeration datatype of the specified member in 
	// this compound datatype.
	EnumType getMemberEnumType( int member_num ) const;

	// Returns the compound datatype of the specified member in 
	// this compound datatype.
	CompType getMemberCompType( int member_num ) const;

	// Returns the integer datatype of the specified member in 
	// this compound datatype.
	IntType getMemberIntType( int member_num ) const;

	// Returns the floating-point datatype of the specified member in 
	// this compound datatype.
	FloatType getMemberFloatType( int member_num ) const;

	// Returns the string datatype of the specified member in 
	// this compound datatype.
	StrType getMemberStrType( int member_num ) const;

	// Adds a new member to this compound datatype.
	void insertMember( const string name, size_t offset, const DataType& new_member ) const;

	// Recursively removes padding from within this compound datatype. 
	void pack() const;

	// Default constructor
	CompType();

	// Creates a compound datatype using an existing id
	CompType( const hid_t existing_id );

	// Copy constructor - makes a copy of original object
	CompType( const CompType& original );

	virtual ~CompType();

   private:
	// Contains common code that is used by the member functions
	// getMemberXxxType
	hid_t p_getMemberType( int member_num ) const; 
};
#ifndef H5_NO_NAMESPACE
}
#endif
#endif
