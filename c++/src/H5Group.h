#ifndef _H5Group_H
#define _H5Group_H

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

class Group : public H5Object, public CommonFG {
   public:
	// default constructor
	Group();

	// Copy constructor: makes a copy of the original object
	Group( const Group& original );

	// for CommonFG to get the file id
	virtual hid_t getLocId() const;

        // Throw group exception
        virtual void throwException() const;

	// Used by the API to appropriately close a group
	void p_close() const;

	virtual ~Group();

        // Creates a copy of an existing Group using its id
        // (used only by template functions in FGtemplates.h
	// to return a Group; will not be published; maybe, use friend???)
        Group( const hid_t group_id );

};
#ifndef H5_NO_NAMESPACE
}
#endif
#endif
