#ifndef _H5PropList_H
#define _H5PropList_H

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

class PropList : public IdComponent {
   public:
	// Default property list
        static const PropList DEFAULT;

	// Creates a property list given the property list type.
	PropList( H5P_class_t type );

	// Default constructor: creates a PropList object - this object
	// does not represent any property list yet.
	PropList();

	// Copy constructor: creates a copy of a PropList object.
	PropList( const PropList& original );

	// Makes a copy of the given property list.
	void copy( const PropList& like_plist );

	// Make a copy of the given property list using assignment statement
	//PropList& operator=( const PropList& rhs );

	// Sets and gets PropList's data member
        //hid_t getId () const;
        //void setId( hid_t new_plist_id );

	// Gets the class of this property list, i.e. H5P_FILE_CREATE,
	// H5P_FILE_ACCESS, ...
	H5P_class_t getClass() const;

	// Creates a default property list or creates a copy of an 
	// existing property list giving the property list id
	PropList( const hid_t plist_id );

	// Used by the API to close the property list
	void p_close() const;

	virtual ~PropList();
};

#ifndef H5_NO_NAMESPACE
}
#endif
#endif  // _H5PropList_H
