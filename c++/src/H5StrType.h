#ifndef _H5StrType_H
#define _H5StrType_H

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

class StrType : public AtomType {
   public:
	// default constructor 
	StrType();

	// Creates a string type using a predefined type
	StrType( const PredType& pred_type );

	// Creates a string datatype using an existing id
	StrType( const hid_t existing_id );

	// Copy constructor - makes a copy of the original object
	StrType( const StrType& original );

        // Gets the string datatype of the specified dataset 
	StrType( const DataSet& dataset );

	// Retrieves the character set type of this string datatype. 
	H5T_cset_t getCset() const;

	// Sets character set to be used. 
	void setCset( H5T_cset_t cset ) const;

	// Retrieves the string padding method for this string datatype. 
	H5T_str_t getStrpad() const;

	// Defines the storage mechanism for character strings. 
	void setStrpad( H5T_str_t strpad ) const;

	virtual ~StrType();
};
#ifndef H5_NO_NAMESPACE
}
#endif
#endif
