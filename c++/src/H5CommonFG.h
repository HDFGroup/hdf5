// C++ informative line for the emacs editor: -*- C++ -*-
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

#ifndef __CommonFG_H
#define __CommonFG_H

namespace H5 {

/*! \class CommonFG
    \brief \a CommonFG was an abstract base class of H5File and H5Group.

    It provided common operations of H5File and H5Group.
    In release 1.10.1, the class structure is modified.
    As a result, member functions of CommonFG are moved to Group.
*/
class H5_DLLCPP CommonFG {
   public:
#ifndef DOXYGEN_SHOULD_SKIP_THIS
	// Default constructor.
	CommonFG();

	// Noop destructor.
	virtual ~CommonFG();

    protected:
	virtual void p_setId(const hid_t new_id) = 0;

#endif // DOXYGEN_SHOULD_SKIP_THIS

}; // end of CommonFG declaration
}
#endif // __CommonFG_H

