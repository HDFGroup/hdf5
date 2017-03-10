/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include <string>

#include "H5Include.h"
#include "H5Exception.h"
#include "H5IdComponent.h"
#include "H5PropList.h"
#include "H5LaccProp.h"

namespace H5 {

#ifndef DOXYGEN_SHOULD_SKIP_THIS
// This DOXYGEN_SHOULD_SKIP_THIS block is a work-around approach to control
// the order of creation and deletion of the global constants.  See Design Notes
// in "H5PredType.cpp" for information.

// Initialize a pointer for the constant
LinkAccPropList* LinkAccPropList::DEFAULT_ = 0;

//--------------------------------------------------------------------------
// Function:    LinkAccPropList::getConstant
//              Creates a LinkAccPropList object representing the HDF5 constant
//              H5P_LINK_ACCESS, pointed to by LinkAccPropList::DEFAULT_
// exception    H5::PropListIException
// Description
//              If LinkAccPropList::DEFAULT_ already points to an allocated
//              object, throw a PropListIException.  This scenario should not
//              happen.
// Programmer   Binh-Minh Ribler - 2015
//--------------------------------------------------------------------------
LinkAccPropList* LinkAccPropList::getConstant()
{
    // Tell the C library not to clean up, H5Library::termH5cpp will call
    // H5close - more dependency if use H5Library::dontAtExit()
    if (!IdComponent::H5dontAtexit_called)
    {
        (void) H5dont_atexit();
        IdComponent::H5dontAtexit_called = true;
    }

    // If the constant pointer is not allocated, allocate it. Otherwise,
    // throw because it shouldn't be.
    if (DEFAULT_ == 0)
        DEFAULT_ = new LinkAccPropList(H5P_LINK_ACCESS);
    else
        throw PropListIException("LinkAccPropList::getConstant", "LinkAccPropList::getConstant is being invoked on an allocated DEFAULT_");
    return(DEFAULT_);
}

//--------------------------------------------------------------------------
// Function:    LinkAccPropList::deleteConstants
// Purpose:     Deletes the constant object that LinkAccPropList::DEFAULT_
//              points to.
// exception    H5::PropListIException
// Programmer   Binh-Minh Ribler - 2015
//--------------------------------------------------------------------------
void LinkAccPropList::deleteConstants()
{
    if (DEFAULT_ != 0)
        delete DEFAULT_;
}

//--------------------------------------------------------------------------
// Purpose:	Constant for default property
//--------------------------------------------------------------------------
const LinkAccPropList& LinkAccPropList::DEFAULT = *getConstant();

#endif // DOXYGEN_SHOULD_SKIP_THIS

//--------------------------------------------------------------------------
// Function:	Default Constructor
///\brief	Creates a file access property list
// Programmer:	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
LinkAccPropList::LinkAccPropList() : PropList( H5P_LINK_ACCESS ) {}

//--------------------------------------------------------------------------
// Function:	LinkAccPropList copy constructor
///\brief	Copy Constructor: makes a copy of the original
///\param	original - IN: LinkAccPropList instance to copy
// Programmer:	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
LinkAccPropList::LinkAccPropList(const LinkAccPropList& original) : PropList(original) {}

//--------------------------------------------------------------------------
// Function:	LinkAccPropList overloaded constructor
///\brief	Creates a file access property list using the id of an
///		existing one.
// Programmer:  Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
LinkAccPropList::LinkAccPropList(const hid_t plist_id) : PropList(plist_id) {}

//--------------------------------------------------------------------------
// Function:	LinkAccPropList destructor
///\brief	Noop destructor
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
LinkAccPropList::~LinkAccPropList() {}

} // end namespace
