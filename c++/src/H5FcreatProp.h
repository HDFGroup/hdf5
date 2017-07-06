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

#ifndef __H5FileCreatPropList_H
#define __H5FileCreatPropList_H

namespace H5 {

/*! \class FileCreatPropList
    \brief Class FileCreatPropList inherits from PropList and provides
    wrappers for the HDF5 file create property list.
*/
//  Inheritance: PropList -> IdComponent
class H5_DLLCPP FileCreatPropList : public PropList {
   public:
        ///\brief Default file creation property list.
        static const FileCreatPropList& DEFAULT;

        // Creates a file create property list.
        FileCreatPropList();

        // Retrieves version information for various parts of a file.
        void getVersion(unsigned& super, unsigned& freelist, unsigned& stab, unsigned& shhdr) const;

        // Sets the userblock size field of a file creation property list.
        void setUserblock(hsize_t size) const;

        // Gets the size of a user block in this file creation property list.
        hsize_t getUserblock() const;

        // Retrieves the size-of address and size quantities stored in a
        // file according to this file creation property list.
        void getSizes(size_t& sizeof_addr, size_t& sizeof_size) const;

        // Sets file size-of addresses and sizes.
        void setSizes(size_t sizeof_addr = 4, size_t sizeof_size = 4) const;

        // Retrieves the size of the symbol table B-tree 1/2 rank and the
        // symbol table leaf node 1/2 size.
        void getSymk(unsigned& int_nodes_k, unsigned& leaf_nodes_k) const;

        // Sets the size of parameters used to control the symbol table nodes.
        void setSymk(unsigned int_nodes_k, unsigned leaf_nodes_k) const;

        // Returns the 1/2 rank of an indexed storage B-tree.
        unsigned getIstorek() const;

        // Sets the size of parameter used to control the B-trees for
        // indexing chunked datasets.
        void setIstorek(unsigned ik) const;

        ///\brief Returns this class name.
        virtual H5std_string fromClass () const { return("FileCreatPropList"); }

        // Copy constructor: creates a copy of a FileCreatPropList object.
        FileCreatPropList(const FileCreatPropList& orig);

        // Creates a copy of an existing file create property list
        // using the property list id.
        FileCreatPropList (const hid_t plist_id);

        // Noop destructor
        virtual ~FileCreatPropList();

#ifndef DOXYGEN_SHOULD_SKIP_THIS

        // Deletes the global constant, should only be used by the library
        static void deleteConstants();

    private:
        static FileCreatPropList* DEFAULT_;

        // Creates the global constant, should only be used by the library
        static FileCreatPropList* getConstant();

#endif // DOXYGEN_SHOULD_SKIP_THIS

}; // end of FileCreatPropList
} // namespace H5

#endif // __H5FileCreatPropList_H
