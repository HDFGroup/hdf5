// C++ informative line for the emacs editor: -*- C++ -*-
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#ifndef __H5Location_H
#define __H5Location_H

#include "H5Classes.h"          // constains forward class declarations

namespace H5 {

class H5Location;  // forward declaration for UserData4Aiterate

// Define the operator function pointer for H5Aiterate().
typedef void (*attr_operator_t)(H5Location& loc/*in*/,
                                 const H5std_string attr_name/*in*/,
                                 void *operator_data/*in,out*/);

class UserData4Aiterate { // user data for attribute iteration
   public:
        attr_operator_t op;
        void* opData;
        H5Location* location;
};

/*! \class H5Location
    \brief H5Location is an abstract base class, providing a collection of
    wrappers of the C functions that take a location identifier, which can be
    either file, group, dataset, attribute, or named datatype.
*/
//  Inheritance: IdComponent
class H5_DLLCPP H5Location : public IdComponent {
   public:
        // Determines the number of attributes belong to this object.
        int getNumAttrs() const;

        // Checks if a link of a given name exists in this location
        bool nameExists(const char* name, const LinkAccPropList& lapl = LinkAccPropList::DEFAULT) const;
        bool nameExists(const H5std_string& name, const LinkAccPropList& lapl = LinkAccPropList::DEFAULT) const;

        // Flushes all buffers associated with this location to disk.
        void flush(H5F_scope_t scope) const;

        // Gets the name of the file, specified by this location.
        H5std_string getFileName() const;

        // Retrieves information about an object at this location
            // specified by location
        void getObjectInfo(H5O_info_t *oinfo) const;
            // specified by the object's name
        void getObjectInfo(const char *name, H5O_info_t *oinfo,
                const LinkAccPropList& lapl = LinkAccPropList::DEFAULT) const;
        void getObjectInfo(const H5std_string& name, H5O_info_t *oinfo,
                const LinkAccPropList& lapl = LinkAccPropList::DEFAULT) const;

#ifndef H5_NO_DEPRECATED_SYMBOLS
        // Retrieves the type of object that an object reference points to.
        H5G_obj_t getObjType(void *ref, H5R_type_t ref_type = H5R_OBJECT) const;
#endif /* H5_NO_DEPRECATED_SYMBOLS */

        // Retrieves the type of object that an object reference points to.
        H5O_type_t getRefObjType(void *ref, H5R_type_t ref_type = H5R_OBJECT) const;
        // Note: getRefObjType deprecates getObjType, but getObjType's name is
        // misleading, so getRefObjType is used in the new function instead.

        // Returns the object header version of an object
        unsigned objVersion() const;

        // Sets the comment for an HDF5 object specified by its name.
        void setComment(const char* name, const char* comment) const;
        void setComment(const H5std_string& name, const H5std_string& comment) const;
        void setComment(const char* comment) const;
        void setComment(const H5std_string& comment) const;

        // Retrieves comment for the HDF5 object specified by its name.
        ssize_t getComment(const char* name, size_t buf_size, char* comment) const;
        H5std_string getComment(const char* name, size_t buf_size=0) const;
        H5std_string getComment(const H5std_string& name, size_t buf_size=0) const;

        // Removes the comment for the HDF5 object specified by its name.
        void removeComment(const char* name) const;
        void removeComment(const H5std_string& name) const;

        // Creates a reference to a named object or to a dataset region
        // in this object.
        void reference(void* ref, const char* name, 
                        H5R_type_t ref_type = H5R_OBJECT) const;
        void reference(void* ref, const H5std_string& name, 
                        H5R_type_t ref_type = H5R_OBJECT) const;
        void reference(void* ref, const char* name, const DataSpace& dataspace,
                        H5R_type_t ref_type = H5R_DATASET_REGION) const;
        void reference(void* ref, const H5std_string& name, const DataSpace& dataspace,
                        H5R_type_t ref_type = H5R_DATASET_REGION) const;

        // Open a referenced object whose location is specified by either
        // a file, an HDF5 object, or an attribute.
        void dereference(const H5Location& loc, const void* ref, H5R_type_t ref_type = H5R_OBJECT);
        void dereference(const Attribute& attr, const void* ref, H5R_type_t ref_type = H5R_OBJECT);

        // Retrieves a dataspace with the region pointed to selected.
        DataSpace getRegion(void *ref, H5R_type_t ref_type = H5R_DATASET_REGION) const;

        // Opens an object at this location, without knowing the object type.
        hid_t openObjId(const char* name, const LinkAccPropList& lapl = LinkAccPropList::DEFAULT) const;
        hid_t openObjId(const H5std_string& name, const LinkAccPropList& lapl = LinkAccPropList::DEFAULT) const;

        // Closes an object opened by openObjId()
        static void closeObjId(hid_t obj_id);

        // Creates a soft link from link_name to target_name.
        void link(const char *target_name, const char *link_name,
             const LinkCreatPropList& lcpl = LinkCreatPropList::DEFAULT,
             const LinkAccPropList& lapl = LinkAccPropList::DEFAULT) const;
        void link(const H5std_string& target_name,
             const H5std_string& link_name,
             const LinkCreatPropList& lcpl = LinkCreatPropList::DEFAULT,
             const LinkAccPropList& lapl = LinkAccPropList::DEFAULT) const;

        // Creates a hard link from new_name to curr_name.
        void link(const char *curr_name,
             const H5Location& new_loc, const char *new_name,
             const LinkCreatPropList& lcpl = LinkCreatPropList::DEFAULT,
             const LinkAccPropList& lapl = LinkAccPropList::DEFAULT) const;
        void link(const H5std_string& curr_name,
             const H5Location& new_loc, const H5std_string& new_name,
             const LinkCreatPropList& lcpl = LinkCreatPropList::DEFAULT,
             const LinkAccPropList& lapl = LinkAccPropList::DEFAULT) const;

        // Creates a hard link from new_name to curr_name in same location.
        void link(const char *curr_name,
             const hid_t same_loc, const char *new_name,
             const LinkCreatPropList& lcpl = LinkCreatPropList::DEFAULT,
             const LinkAccPropList& lapl = LinkAccPropList::DEFAULT) const;
        void link(const H5std_string& curr_name,
             const hid_t same_loc, const H5std_string& new_name,
             const LinkCreatPropList& lcpl = LinkCreatPropList::DEFAULT,
             const LinkAccPropList& lapl = LinkAccPropList::DEFAULT) const;

        // Removes the specified link from this location.
        void unlink(const char *link_name,
            const LinkAccPropList& lapl = LinkAccPropList::DEFAULT) const;
        void unlink(const H5std_string& link_name,
            const LinkAccPropList& lapl = LinkAccPropList::DEFAULT) const;

        // Copies a link from this location to another.
        void copyLink(const char *src_name,
             const H5Location& dst, const char *dst_name,
             const LinkCreatPropList& lcpl = LinkCreatPropList::DEFAULT,
             const LinkAccPropList& lapl = LinkAccPropList::DEFAULT) const;
        void copyLink(const H5std_string& src_name,
             const H5Location& dst, const H5std_string& dst_name,
             const LinkCreatPropList& lcpl = LinkCreatPropList::DEFAULT,
             const LinkAccPropList& lapl = LinkAccPropList::DEFAULT) const;

        // Makes a copy of a link in the same location.
        void copyLink(const char *src_name, const char *dst_name,
             const LinkCreatPropList& lcpl = LinkCreatPropList::DEFAULT,
             const LinkAccPropList& lapl = LinkAccPropList::DEFAULT) const;
        void copyLink(const H5std_string& src_name,
             const H5std_string& dst_name,
             const LinkCreatPropList& lcpl = LinkCreatPropList::DEFAULT,
             const LinkAccPropList& lapl = LinkAccPropList::DEFAULT) const;

        // Renames a link in this location and moves to a new location.
        void moveLink(const char* src_name,
            const H5Location& dst, const char* dst_name,
            const LinkCreatPropList& lcpl = LinkCreatPropList::DEFAULT,
            const LinkAccPropList& lapl = LinkAccPropList::DEFAULT) const;
        void moveLink(const H5std_string& src_name,
            const H5Location& dst, const H5std_string& dst_name,
            const LinkCreatPropList& lcpl = LinkCreatPropList::DEFAULT,
            const LinkAccPropList& lapl = LinkAccPropList::DEFAULT) const;

        // Renames a link in this location.
        void moveLink(const char* src_name, const char* dst_name,
            const LinkCreatPropList& lcpl = LinkCreatPropList::DEFAULT,
            const LinkAccPropList& lapl = LinkAccPropList::DEFAULT) const;
        void moveLink(const H5std_string& src_name,
            const H5std_string& dst_name,
            const LinkCreatPropList& lcpl = LinkCreatPropList::DEFAULT,
            const LinkAccPropList& lapl = LinkAccPropList::DEFAULT) const;

        // Returns the information of the named link.
        H5L_info_t getLinkInfo(const char* link_name, const LinkAccPropList& lapl = LinkAccPropList::DEFAULT) const;
        H5L_info_t getLinkInfo(const H5std_string& link_name, const LinkAccPropList& lapl = LinkAccPropList::DEFAULT) const;

        // Returns the value of a symbolic link.
        H5std_string getLinkval(const char* link_name, size_t size=0) const;
        H5std_string getLinkval(const H5std_string& link_name, size_t size=0) const;

        ///\brief Returns an identifier. (pure virtual)
        virtual hid_t getId() const = 0;

/***************************************************************************
                           Notes for H5A wrappers
                           ======================
        These H5A wrappers are marked "deprecated" in 1.8.19.
        They are moved to H5Object to prevent the object id from being
        passed in to H5A APIs.
        Updated: they are removed from source code in 1.8.21.
***************************************************************************/

        // Creates an attribute for the specified object at this location
        // PropList is currently not used, so always be default.
        // Deprecated
        //virtual Attribute createAttribute(const char* name, const DataType& type, const DataSpace& space, const PropList& create_plist = PropList::DEFAULT) const;
        //virtual Attribute createAttribute(const H5std_string& name, const DataType& type, const DataSpace& space, const PropList& create_plist = PropList::DEFAULT) const;

        // Given its name, opens the attribute that belongs to an object at
        // this location.
        // Deprecated
        //virtual Attribute openAttribute(const char* name) const;
        //virtual Attribute openAttribute(const H5std_string& name) const;

        // Given its index, opens the attribute that belongs to an object at
        // this location.
        //virtual Attribute openAttribute(const unsigned int idx) const; // Deprecated

        // Iterate user's function over the attributes at this location.
        virtual int iterateAttrs(attr_operator_t user_op, unsigned* idx = NULL,
                                 void* op_data = NULL); // Deprecated

        // Checks whether the named attribute exists at this location.
        // Deprecated
        //virtual bool attrExists(const char* name) const;
        //virtual bool attrExists(const H5std_string& name) const;

        // Renames the named attribute to a new name.
        // Deprecated
        //virtual void renameAttr(const char* oldname, const char* newname) const;
        //virtual void renameAttr(const H5std_string& oldname, const H5std_string& newname) const;

        // Removes the named attribute from this location.
        // Deprecated
        //virtual void removeAttr(const char* name) const;
        //virtual void removeAttr(const H5std_string& name) const;

/**************************** End of H5A note *******************************/

   protected:
#ifndef DOXYGEN_SHOULD_SKIP_THIS
        // Default constructor
        H5Location();

        // *** Deprecation warning ***
        // The following two constructors are no longer appropriate after the
        // data member "id" had been moved to the sub-classes.
        // The copy constructor is a noop and is removed in 1.8.15 and the
        // other will be removed from 1.10 release, and then from 1.8 if its
        // removal does not raise any problems in two 1.10 releases.

        // Creates a copy of an existing object giving the location id.
        H5Location(const hid_t loc_id);

        // Creates a reference to an HDF5 object or a dataset region.
        void p_reference(void* ref, const char* name, hid_t space_id, H5R_type_t ref_type) const;

        // Dereferences a ref into an HDF5 id.
        hid_t p_dereference(hid_t loc_id, const void* ref, H5R_type_t ref_type, const char* from_func);

#ifndef H5_NO_DEPRECATED_SYMBOLS
        // Retrieves the type of object that an object reference points to.
        H5G_obj_t p_get_obj_type(void *ref, H5R_type_t ref_type) const;
#endif /* H5_NO_DEPRECATED_SYMBOLS */

        // Retrieves the type of object that an object reference points to.
        H5O_type_t p_get_ref_obj_type(void *ref, H5R_type_t ref_type) const;

        // Sets the identifier of this object to a new value. - this one
        // doesn't increment reference count
        virtual void p_setId(const hid_t new_id) = 0;

#endif // DOXYGEN_SHOULD_SKIP_THIS

        // Noop destructor.
        virtual ~H5Location();

}; // end of H5Location
} // namespace H5

#endif // __H5Location_H

// Modification
//      Oct 1, 2013 -BMR
//          H5Location is added in version 1.8.12.
//          Most of these methods were in H5Object but are now moved here
//          because a location can be a file, group, dataset, or named datatype.
//      May 04, 2017 -BMR
//          Wrappers for H5A functions are copied to H5Object because H5A
//          functions do not take an attribute id as loc_id.  The original
//          wrappers will be deprecated in future releases.
