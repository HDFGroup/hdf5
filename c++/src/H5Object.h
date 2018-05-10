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

#ifndef __H5Object_H
#define __H5Object_H

namespace H5 {

/*! \class H5Object
    \brief Class H5Object is a bridge between H5Location and DataSet, DataType,
     and Group.

    Modification:
        Sept 18, 2012: Added class H5Location in between IdComponent and
                H5Object.  An H5File now inherits from H5Location.  All HDF5
                wrappers in H5Object are moved up to H5Location.  H5Object
                is left mostly empty for future wrappers that are only for
                group, dataset, and named datatype.  Note that the reason for
                adding H5Location instead of simply moving H5File to be under
                H5Object is H5File is not an HDF5 object, and renaming H5Object
                to H5Location will risk breaking user applications.  -BMR
        Apr 2, 2014: Added wrapper getObjName for H5Iget_name -BMR
*/
//  Inheritance: H5Location -> IdComponent

class H5_DLLCPP H5Object : public H5Location {
   public:
        // Creates an attribute for the specified object
        // PropList is currently not used, so always be default.
        Attribute createAttribute(const char* name, const DataType& type, const DataSpace& space, const PropList& create_plist = PropList::DEFAULT) const;
        Attribute createAttribute(const H5std_string& name, const DataType& type, const DataSpace& space, const PropList& create_plist = PropList::DEFAULT) const;

        // Given its name, opens the attribute that belongs to this object.
        Attribute openAttribute(const char* name) const;
        Attribute openAttribute(const H5std_string& name) const;

        // Given its index, opens the attribute that belongs to this object.
        Attribute openAttribute(const unsigned int idx) const;

        // Determines the number of attributes belong to this object.
        int getNumAttrs() const;

        // Checks whether the named attribute exists for this object.
        bool attrExists(const char* name) const;
        bool attrExists(const H5std_string& name) const;

        // Renames the named attribute of this object to a new name.
        void renameAttr(const char* oldname, const char* newname) const;
        void renameAttr(const H5std_string& oldname, const H5std_string& newname) const;

        // Removes the named attribute from this object.
        void removeAttr(const char* name) const;
        void removeAttr(const H5std_string& name) const;

        // Gets the name of this HDF5 object, i.e., Group, DataSet, or
        // DataType.
        ssize_t getObjName(char *obj_name, size_t buf_size = 0) const;
        ssize_t getObjName(H5std_string& obj_name, size_t len = 0) const;
        H5std_string getObjName() const;

#ifndef DOXYGEN_SHOULD_SKIP_THIS
        // Noop destructor.
        virtual ~H5Object();

   protected:
        // Default constructor
        H5Object();

        // *** Deprecation warning ***
        // The following two constructors are no longer appropriate after the
        // data member "id" had been moved to the sub-classes.
        // The copy constructor is a noop and is removed in 1.8.15 and the
        // other will be removed from 1.10 release, and then from 1.8 if its
        // removal does not raise any problems in two 1.10 releases.

        // Creates a copy of an existing object giving the object id
        // H5Object(const hid_t object_id);

        // Copy constructor: makes copy of an H5Object object.
        // H5Object(const H5Object& original);

#endif // DOXYGEN_SHOULD_SKIP_THIS

}; // end of H5Object
} // namespace H5

#endif // __H5Object_H
