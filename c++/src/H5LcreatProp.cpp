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

#include "H5Include.h"
#include "H5Exception.h"
#include "H5IdComponent.h"
#include "H5PropList.h"
#include "H5StrcreatProp.h"
#include "H5LcreatProp.h"

namespace H5 {

#ifndef DOXYGEN_SHOULD_SKIP_THIS
// This DOXYGEN_SHOULD_SKIP_THIS block is a work-around approach to control
// the order of creation and deletion of the global constants.  See Design Notes
// in "H5PredType.cpp" for information.

// Initialize a pointer for the constant
LinkCreatPropList *LinkCreatPropList::DEFAULT_ = 0;

//--------------------------------------------------------------------------
// Function:    LinkCreatPropList::getConstant
//              Creates a LinkCreatPropList object representing the HDF5 constant
//              H5P_LINK_CREATE, pointed to by LinkCreatPropList::DEFAULT_
// exception    H5::PropListIException
// Description
//              If LinkCreatPropList::DEFAULT_ already points to an allocated
//              object, throw a PropListIException.  This scenario should not
//              happen.
// May 2018
//--------------------------------------------------------------------------
LinkCreatPropList *
LinkCreatPropList::getConstant()
{
    // Tell the C library not to clean up, H5Library::termH5cpp will call
    // H5close - more dependency if use H5Library::dontAtExit()
    if (!IdComponent::H5dontAtexit_called) {
        (void)H5dont_atexit();
        IdComponent::H5dontAtexit_called = true;
    }

    // If the constant pointer is not allocated, allocate it. Otherwise,
    // throw because it shouldn't be.
    if (DEFAULT_ == 0)
        DEFAULT_ = new LinkCreatPropList(H5P_LINK_CREATE);
    else
        throw PropListIException("LinkCreatPropList::getConstant",
                                 "LinkCreatPropList::getConstant is being invoked on an allocated DEFAULT_");
    return (DEFAULT_);
}

//--------------------------------------------------------------------------
// Function:    LinkCreatPropList::deleteConstants
// Purpose:     Deletes the constant object that LinkCreatPropList::DEFAULT_
//              points to.
// exception    H5::PropListIException
// May 2018
//--------------------------------------------------------------------------
void
LinkCreatPropList::deleteConstants()
{
    if (DEFAULT_ != 0)
        delete DEFAULT_;
}

//--------------------------------------------------------------------------
// Purpose:     Constant for default link creation property
//--------------------------------------------------------------------------
const LinkCreatPropList &LinkCreatPropList::DEFAULT = *getConstant();

#endif // DOXYGEN_SHOULD_SKIP_THIS

//--------------------------------------------------------------------------
// Function:    Default Constructor
///\brief       Creates a file access property list
// May 2018
//--------------------------------------------------------------------------
LinkCreatPropList::LinkCreatPropList() : StrCreatPropList(H5P_LINK_CREATE)
{
}

//--------------------------------------------------------------------------
// Function:    LinkCreatPropList copy constructor
///\brief       Copy constructor: same HDF5 object as \a original
///\param       original - IN: LinkCreatPropList instance to copy
// May 2018
//--------------------------------------------------------------------------
LinkCreatPropList::LinkCreatPropList(const LinkCreatPropList &original) : StrCreatPropList(original)
{
}

//--------------------------------------------------------------------------
// Function:    LinkCreatPropList overloaded constructor
///\brief       Creates a file access property list using the id of an
///             existing one.
// May 2018
//--------------------------------------------------------------------------
LinkCreatPropList::LinkCreatPropList(const hid_t plist_id) : StrCreatPropList(plist_id)
{
}

//--------------------------------------------------------------------------
// Function:    LinkCreatPropList destructor
///\brief       Noop destructor
// May 2018
//--------------------------------------------------------------------------
LinkCreatPropList::~LinkCreatPropList()
{
}

} // namespace H5
