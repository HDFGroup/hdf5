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

#ifndef __H5StrCreatPropList_H
#define __H5StrCreatPropList_H

namespace H5 {

// Class StrCreatPropList is not user-accessible.  It provides
// wrappers for the HDF5 string creation properties.

// Inheritance: PropList -> IdComponent
class H5_DLLCPP StrCreatPropList : public PropList {
   public:
        // There is no StrCreatPropList::DEFAULT;

        // Returns this class name.
        virtual H5std_string fromClass () const { return("StrCreatPropList"); }

        // Sets the character encoding of the string.
        void setCharEncoding(H5T_cset_t encoding) const;

        // Gets the character encoding of the string.
        H5T_cset_t getCharEncoding() const;

#ifndef DOXYGEN_SHOULD_SKIP_THIS
    protected:
        // Creates a string creation property list.
        StrCreatPropList();

        // Copy constructor: same as the original StrCreatPropList.
        StrCreatPropList(const StrCreatPropList& original);

        // Creates a copy of an existing string creation property list
        // using the property list id.
        StrCreatPropList(const hid_t plist_id);

        // Noop destructor
        virtual ~StrCreatPropList() {};

#endif // DOXYGEN_SHOULD_SKIP_THIS
}; // end of StrCreatPropList

} // namespace H5

#endif // __H5StrCreatPropList_H
