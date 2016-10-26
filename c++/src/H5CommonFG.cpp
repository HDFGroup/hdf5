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
#include "H5FaccProp.h"
#include "H5FcreatProp.h"
#include "H5OcreatProp.h"
#include "H5DxferProp.h"
#include "H5DcreatProp.h"
#include "H5Location.h"
#include "H5Object.h"
#include "H5CommonFG.h"
#include "H5Group.h"
#include "H5AbstractDs.h"
#include "H5DataSpace.h"
#include "H5DataSet.h"
#include "H5File.h"
#include "H5Alltypes.h"
#include "H5private.h"		// for HDstrcpy

// There are a few comments that are common to most of the functions
// defined in this file so they are listed here.
// - getLocId is called by all functions, that call a C API, to get
//   the location id, which can be either a file id or a group id.
//   This function is pure virtual and it's up to H5File and Group
//   to call the right getId() - although, as the structure of the
//   library at this time, getId() is basically the IdComponent::getId()
// - when a failure returned by the C API, the functions will call
//   throwwException, which is a pure virtual function and is implemented
//   by H5File to throw a FileIException and by Group to throw a
//   GroupIException.
// December 2000

namespace H5 {

#ifndef DOXYGEN_SHOULD_SKIP_THIS
//--------------------------------------------------------------------------
// Function:	CommonFG default constructor
///\brief	Default constructor.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
CommonFG::CommonFG() {}

//--------------------------------------------------------------------------
// Function:	CommonFG destructor
///\brief	Noop destructor.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
CommonFG::~CommonFG() {}

#endif // DOXYGEN_SHOULD_SKIP_THIS

}
