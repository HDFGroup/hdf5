#ifndef _AbstractDs_H
#define _AbstractDs_H

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif
class AbstractDs : public H5Object {
   public:
	// Copy constructor
	AbstractDs( const AbstractDs& original );

	// Gets the dataspace of this abstract dataset - pure virtual
	virtual DataSpace getSpace() const = 0;

        // Gets the class of the datatype that is used by this abstract 
	// dataset        
	H5T_class_t getTypeClass() const;

	// Gets a copy the datatype of this abstract dataset.  Note that 
	// this datatype is a generic one and can only be accessed via 
	// generic member functions, i.e., member functions belong to 
	// DataType.  To get specific datatype, i.e. EnumType, 
	// FloatType, etc..., use the specific functions instead .
	DataType getDataType() const;

        // Gets a copy of the specific datatype of this abstract dataset -
	// overloading for subtypes.
        EnumType getEnumType() const;
        CompType getCompType() const;
        IntType getIntType() const;
        FloatType getFloatType() const;
        StrType getStrType() const;

	virtual ~AbstractDs() {}; 

   protected:
	// Default constructor
	AbstractDs();

	// Constructor that takes an attribute id or a dataset id.
	AbstractDs( const hid_t ds_id );

   private:
	// This member function is implemented by DataSet and Attribute
	virtual hid_t p_getType() const = 0;

	// This member function is implemented by DataSet and Attribute
        virtual void p_close() const = 0;
};
#ifndef H5_NO_NAMESPACE
}
#endif
#endif // _AbstractDs_H
