// C++ informative line for the emacs editor: -*- C++ -*-
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  * Copyright by the Board of Trustees of the University of Illinois.         *
  * All rights reserved.                                                      *
  *                                                                           *
  * This file is part of HDF5.  The full HDF5 copyright notice, including     *
  * terms governing use, modification, and redistribution, is contained in    *
  * the files COPYING and Copyright.html.  COPYING can be found at the root   *
  * of the source code distribution tree; Copyright.html can be found at the  *
  * root level of an installed copy of the electronic HDF5 document set and   *
  * is linked from the top-level documents page.  It can also be found at     *
  * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
  * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#ifndef _H5Group_H
#define _H5Group_H

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

class H5_DLLCPP Group : public H5Object, public CommonFG {
   public:
	// default constructor
	Group();

	// Copy constructor: makes a copy of the original object
	Group( const Group& original );

	// for CommonFG to get the file id
	virtual hid_t getLocId() const;

        // Throw group exception
        virtual void throwException(const string& func_name, const string& msg) const;


	// Used by the API to appropriately close a group
	void p_close() const;

	virtual ~Group();

        // Creates a copy of an existing Group using its id
        // (used only by template functions in FGtemplates.h
	// to return a Group; will not be published; maybe, use friend???)
        Group( const hid_t group_id );

};
#ifndef H5_NO_NAMESPACE
}
#endif
#endif
