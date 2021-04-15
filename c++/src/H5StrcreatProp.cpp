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

#include <string>

#include <iostream>
using std::cerr;
using std::endl;

#include "H5Include.h"
#include "H5Exception.h"
#include "H5IdComponent.h"
#include "H5PropList.h"
#include "H5StrcreatProp.h"

namespace H5 {

#ifndef DOXYGEN_SHOULD_SKIP_THIS
// Currently, StrCreatPropList is an internal base class.

//--------------------------------------------------------------------------
// Function:    StrCreatPropList default constructor
///\brief       Default constructor: Creates a string create property list
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
StrCreatPropList::StrCreatPropList() : PropList(H5P_STRING_CREATE)
{
}

//--------------------------------------------------------------------------
// Function:    StrCreatPropList copy constructor
///\brief       Copy constructor: makes a copy of the original
///             StrCreatPropList object.
///\param       original - IN: StrCreatPropList instance to copy
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
StrCreatPropList::StrCreatPropList(const StrCreatPropList &original) : PropList(original)
{
}

//--------------------------------------------------------------------------
// Function:    StrCreatPropList overloaded constructor
///\brief       Creates a file creation property list using the id of an
///             existing one.
///\param       plist_id - IN: StrCreatPropList id to use
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
StrCreatPropList::StrCreatPropList(const hid_t plist_id) : PropList(plist_id)
{
}

#endif // DOXYGEN_SHOULD_SKIP_THIS

//--------------------------------------------------------------------------
// Function:    StrCreatPropList::setCharEncoding
///\brief       Sets the character encoding of the string.
///\exception   H5::PropListIException
// March 2018
//--------------------------------------------------------------------------
void
StrCreatPropList::setCharEncoding(H5T_cset_t encoding) const
{
    herr_t ret_value = H5Pset_char_encoding(id, encoding);
    // Throw exception if H5Pset_char_encoding returns failure
    if (ret_value < 0) {
        throw PropListIException("StrCreatPropList::setCharEncoding", "H5Pset_char_encoding failed");
    }
}

//--------------------------------------------------------------------------
// Function:    StrCreatPropList::getCharEncoding
///\brief       Gets the character encoding of the string.
///\exception   H5::PropListIException
// March 2018
//--------------------------------------------------------------------------
H5T_cset_t
StrCreatPropList::getCharEncoding() const
{
    H5T_cset_t encoding;
    herr_t     ret_value = H5Pget_char_encoding(id, &encoding);
    // Throw exception if H5Pget_char_encoding returns failure
    if (ret_value < 0) {
        throw PropListIException("StrCreatPropList::getCharEncoding", "H5Pget_char_encoding failed");
    }
    return (encoding);
}

} // namespace H5
