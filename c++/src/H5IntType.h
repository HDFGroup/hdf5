#ifndef _H5IntType_H
#define _H5IntType_H

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif
class IntType : public AtomType {
   public:

	// default constructor
	IntType();

	// Creates a integer datatype using an existing id
	IntType( const hid_t existing_id );

	// Creates a integer type using a predefined type
	IntType( const PredType& pred_type );

	// Copy constructor: makes copy of IntType object
	IntType( const IntType& original );

	// Gets the integer datatype of the specified dataset
	IntType( const DataSet& dataset );

	// Retrieves the sign type for an integer type
	H5T_sign_t getSign() const;

	// Sets the sign proprety for an integer type. 
	void setSign( H5T_sign_t sign ) const;

	virtual ~IntType();
};
#ifndef H5_NO_NAMESPACE
}
#endif
#endif
