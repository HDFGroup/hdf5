#ifndef _H5Object_H
#define _H5Object_H

#include "H5Classes.h"		// constains forward class declarations

// H5Object is a baseclass.  It has these subclasses: 
// Group, AbstractDs, and DataType. 
// AbstractDs, in turn, has subclasses DataSet and Attribute.
// DataType, in turn, has several specific datatypes as subclasses.

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

class H5Object;  // forward declaration for UserData4Aiterate

// Define the operator function pointer for H5Aiterate().
typedef void (*attr_operator_t)( H5Object& loc/*in*/, 
				 const string attr_name/*in*/, 
				 void *operator_data/*in,out*/);

class UserData4Aiterate { // user data for attribute iteration
   public:
	unsigned int* idx;
	attr_operator_t op;
	void* opData;
	H5Object* object;
};

// The above part is being moved into Iterator, but not completed

class H5Object : public IdComponent {
   public:
	// Pure virtual function so appropriate close function can 
	// be called by subclasses' for the corresponding HDF5 object
	//virtual void p_close() const = 0;

	// Copy constructor: makes copy of an H5Object object.
	H5Object( const H5Object& original );

	// Flushes all buffers associated with this object to disk
	void flush( H5F_scope_t scope ) const;

	// Assignment operator
	//H5Object& operator=( const H5Object& rhs );

	// Sets and gets H5Object's data member
	//void setId( hid_t new_id );
	//hid_t getId () const;

	// Creates an attribute for a group, dataset, or named datatype.
	// PropList is currently not used, so always be default.
	Attribute createAttribute( const char* name, const DataType& type, const DataSpace& space, const PropList& create_plist = PropList::DEFAULT ) const;
	Attribute createAttribute( const string& name, const DataType& type, const DataSpace& space, const PropList& create_plist = PropList::DEFAULT ) const;

	// Opens an attribute given its name.
	Attribute openAttribute( const string& name ) const;
	Attribute openAttribute( const char* name ) const;

	// Opens an attribute given its index.
	Attribute openAttribute( const unsigned int idx ) const;

	// Iterate user's function over the attributes of this object
	int iterateAttrs( attr_operator_t user_op, unsigned* idx = NULL, void* op_data = NULL );

	// Determines the number of attributes attached to this object.
	int getNumAttrs() const;

	// Removes the named attribute from this object.
	void removeAttr( const string& name ) const;
	void removeAttr( const char* name ) const;

	virtual ~H5Object();

   protected:

	// Default constructor
	H5Object();

	// Creates a copy of an existing object giving the object id
	H5Object( const hid_t object_id );

}; /* end class H5Object */

#ifndef H5_NO_NAMESPACE
}
#endif
#endif
