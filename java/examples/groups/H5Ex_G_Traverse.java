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

/************************************************************
This example shows a way to recursively traverse the file
using H5Literate.  The method shown here guarantees that
the recursion will not enter an infinite loop, but does
not prevent objects from being visited more than once.
The program prints the directory structure of the file
specified in FILE.  The default file used by this example
implements the structure described in the User's Guide,
chapter 4, figure 26.
 ************************************************************/
package examples.groups;

import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;
import hdf.hdf5lib.callbacks.H5L_iterate_t;
import hdf.hdf5lib.callbacks.H5L_iterate_opdata_t;
import hdf.hdf5lib.structs.H5L_info_t;
import hdf.hdf5lib.structs.H5O_info_t;
import examples.groups.H5Ex_G_Iterate.H5O_type;

class opdata implements H5L_iterate_opdata_t {
    int recurs;
    opdata prev;
    H5O_token_t obj_token;
}

public class H5Ex_G_Traverse {

    private static String FILE = "h5ex_g_traverse.h5";
    public static H5L_iterate_t iter_cb = new H5L_iter_callbackT();

    private static void OpenGroup() {
        long file_id = HDF5Constants.H5I_INVALID_HID;
        H5O_info_t infobuf;
        opdata od = new opdata();

        // Open file and initialize the operator data structure.
        try {
            file_id = H5.H5Fopen(FILE, HDF5Constants.H5F_ACC_RDONLY, HDF5Constants.H5P_DEFAULT);
            if (file_id >= 0) {
                infobuf = H5.H5Oget_info(file_id);
                od.recurs = 0;
                od.prev = null;
                od.obj_token = infobuf.token;
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Print the root group and formatting, begin iteration.
        try {
            System.out.println("/ {");
            // H5L_iterate_t iter_cb = new H5L_iter_callbackT();
            H5.H5Literate(file_id, HDF5Constants.H5_INDEX_NAME, HDF5Constants.H5_ITER_NATIVE, 0L, iter_cb, od);
            System.out.println("}");
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Close and release resources.
        try {
            if (file_id >= 0)
                H5.H5Fclose(file_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static void main(String[] args) {
        H5Ex_G_Traverse.OpenGroup();
    }
}

class H5L_iter_callbackT implements H5L_iterate_t {
    public int callback(long group, String name, H5L_info_t info, H5L_iterate_opdata_t op_data) {

        H5O_info_t infobuf;
        int return_val = 0;
        opdata od = (opdata) op_data; // Type conversion
        int spaces = 2 * (od.recurs + 1); // Number of white spaces to prepend to output.

        // Get type of the object and display its name and type.
        // The name of the object is passed to this function by the Library.
        try {
            infobuf = H5.H5Oget_info_by_name(group, name, HDF5Constants.H5P_DEFAULT);

            for (int i = 0; i < spaces; i++)
                System.out.print(" "); // Format output.
            switch (H5O_type.get(infobuf.type)) {
            case H5O_TYPE_GROUP:
                System.out.println("Group: " + name + " { ");
                // Check group object token against linked list of operator
                // data structures. We will always run the check, as the
                // reference count cannot be relied upon if there are
                // symbolic links, and H5Oget_info_by_name always follows
                // symbolic links. Alternatively we could use H5Lget_info
                // and never recurse on groups discovered by symbolic
                // links, however it could still fail if an object's
                // reference count was manually manipulated with
                // H5Odecr_refcount.
                if (group_check(od, infobuf.token)) {
                    for (int i = 0; i < spaces; i++)
                        System.out.print(" ");
                    System.out.println("  Warning: Loop detected!");
                }
                else {
                    // Initialize new object of type opdata and begin
                    // recursive iteration on the discovered
                    // group. The new opdata is given a pointer to the
                    // current one.
                    opdata nextod = new opdata();
                    nextod.recurs = od.recurs + 1;
                    nextod.prev = od;
                    nextod.obj_token = infobuf.token;
                    H5L_iterate_t iter_cb2 = new H5L_iter_callbackT();
                    return_val = H5.H5Literate_by_name(group, name, HDF5Constants.H5_INDEX_NAME,
                            HDF5Constants.H5_ITER_NATIVE, 0L, iter_cb2, nextod, HDF5Constants.H5P_DEFAULT);
                }
                for (int i = 0; i < spaces; i++)
                    System.out.print(" ");
                System.out.println("}");
                break;
            case H5O_TYPE_DATASET:
                System.out.println("Dataset: " + name);
                break;
            case H5O_TYPE_NAMED_DATATYPE:
                System.out.println("Datatype: " + name);
                break;
            default:
                System.out.println("Unknown: " + name);
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        return return_val;
    }

    public boolean group_check(opdata od, H5O_token_t target_token) {
        if (od.obj_token.equals(target_token))
            return true; // Object tokens match
        else if (od.recurs == 0)
            return false; // Root group reached with no matches
        else
            return group_check(od.prev, target_token); // Recursively examine the next node
    }

}
