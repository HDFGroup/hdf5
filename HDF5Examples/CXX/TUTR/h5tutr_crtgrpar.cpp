/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 *  This example illustrates the creation of groups using absolute and
 *  relative names. It is used in the HDF5 Tutorial.
 */

#include <iostream>
using std::cout;
using std::endl;

#include <string>
#include "H5Cpp.h"
using namespace H5;

const H5std_string FILE_NAME("h5tutr_groups.h5");

int
main(void)
{

    // Try block to detect exceptions raised by any of the calls inside it
    try {

        // Turn off the auto-printing when failure occurs so that we can
        // handle the errors appropriately.

        Exception::dontPrint();

        // Create a new file using default properties.

        H5File file(FILE_NAME, H5F_ACC_TRUNC);

        // Create group "MyGroup" in the root group using an absolute name.

        Group group1(file.createGroup("/MyGroup"));

        // Create group "Group_A" in group "MyGroup" using an
        // absolute name.

        Group group2(file.createGroup("/MyGroup/Group_A"));

        // Create group "Group_B" in group "MyGroup" using a
        // relative name.

        Group group3(group1.createGroup("Group_B"));

        // Close the groups and file.

        group1.close();
        group2.close();
        group3.close();
        file.close();

    } // end of try block

    // catch failure caused by the File operations
    catch (FileIException error) {
        error.printErrorStack();
        return -1;
    }

    // catch failure caused by the Group operations
    catch (GroupIException error) {
        error.printErrorStack();
        return -1;
    }

    return 0;
}
