/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.						     *
 * Copyright by the Board of Trustees of the University of Illinois.	     *
 * All rights reserved.							     *
 *	                                                                     *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have	     *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 *  This example illustrates the creation of groups using absolute and
 *  relative names. It is used in the HDF5 Tutorial.
 */

#include <iostream>
#include <string>

#include "H5Cpp.h"

#ifndef H5_NO_NAMESPACE
    using namespace H5;
#ifndef H5_NO_STD
    using std::cout;
    using std::endl;
#endif  // H5_NO_STD
#endif

const H5std_string FILE_NAME("h5tutr_groups.h5");

int main(void)
{

    // Try block to detect exceptions raised by any of the calls inside it
    try
    {
      
	// Turn off the auto-printing when failure occurs so that we can
	// handle the errors appropriately.

	Exception::dontPrint();

	// Create a new file using default properties.

	H5File file(FILE_NAME, H5F_ACC_TRUNC);

	// Create group "MyGroup" in the root group using an absolute name.
	 
	Group group1(file.createGroup( "/MyGroup"));
 
	// Create group "Group_A" in group "MyGroup" using an
	// absolute name.

	Group group2(file.createGroup("/MyGroup/Group_A"));   

	// Create group "Group_B" in group "MyGroup" using a
	// relative name.
  
	Group group3(group1.createGroup ("Group_B"));
 
	// Close the groups and file.

	group1.close();
	group2.close();
	group3.close();
	file.close();
    
    } // end of try block

    // catch failure caused by the File operations
    catch(FileIException error)
    {
	error.printError();
	return -1;
    }

    // catch failure caused by the Group operations
    catch(GroupIException error)
    {
	error.printError();
	return -1;
    }

    return 0;
}


