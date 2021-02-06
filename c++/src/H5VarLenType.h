// C++ informative line for the emacs editor: -*- C++ -*-
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#ifndef __H5VarLenType_H
#define __H5VarLenType_H

namespace H5 {

/*! \class VarLenType
    \brief VarLenType is a derivative of a DataType and operates on HDF5
    C's Variable-length Datatypes.
*/
//  Inheritance: DataType -> H5Object -> H5Location -> IdComponent
class H5_DLLCPP VarLenType : public DataType {
  public:
    // Constructor that creates a variable-length datatype based
    // on the specified base type.
    VarLenType(const DataType &base_type);

    // Deprecated - will be removed after 1.8.20
    VarLenType(const DataType *base_type);

    // Constructors that open a variable-length datatype, given a location.
    VarLenType(const H5Location &loc, const char *name);
    VarLenType(const H5Location &loc, const H5std_string &name);

    // Returns an VarLenType object via DataType* by decoding the
    // binary object description of this type.
    virtual DataType *decode() const;

    ///\brief Returns this class name.
    virtual H5std_string
    fromClass() const
    {
        return ("VarLenType");
    }

    // Copy constructor: makes copy of the original object.
    VarLenType(const VarLenType &original);

    // Constructor that takes an existing id
    VarLenType(const hid_t existing_id);

    // Noop destructor
    virtual ~VarLenType();

    // Default constructor
    VarLenType();

}; // end of VarLenType
} // namespace H5

#endif // __H5VarLenType_H
