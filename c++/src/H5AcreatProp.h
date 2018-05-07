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

#ifndef __H5AttrCreatPropList_H
#define __H5AttrCreatPropList_H

namespace H5 {

/*! \class AttrCreatPropList
    \brief Class AttrCreatPropList inherits from StrCreatPropList and provides
    wrappers for the HDF5 attribute creation property list.
*/
// Inheritance: StrCreatPropList -> PropList -> IdComponent
class H5_DLLCPP AttrCreatPropList : public StrCreatPropList {
   public:
        ///\brief Default attribute creation property list.
        static const AttrCreatPropList& DEFAULT;

        // Creates a attribute creation property list.
        AttrCreatPropList();

        ///\brief Returns this class name.
        virtual H5std_string fromClass () const { return("AttrCreatPropList"); }

        // Copy constructor: same as the original AttrCreatPropList.
        AttrCreatPropList(const AttrCreatPropList& original);

        // Creates a copy of an existing attribute creation property list
        // using the property list id.
        AttrCreatPropList(const hid_t acpl_id);

        // Noop destructor
        virtual ~AttrCreatPropList();

#ifndef DOXYGEN_SHOULD_SKIP_THIS

        // Deletes the global constant, should only be used by the library
        static void deleteConstants();

    private:
        static AttrCreatPropList* DEFAULT_;

        // Creates the global constant, should only be used by the library
        static AttrCreatPropList* getConstant();

#endif // DOXYGEN_SHOULD_SKIP_THIS
}; // end of AttrCreatPropList

} // namespace H5

#endif // __H5AttrCreatPropList_H
