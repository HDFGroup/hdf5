/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the LICENSE file, which can be found at the root of the source code       *
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

import static org.hdfgroup.javahdf5.hdf5_h.*;

import java.lang.foreign.Arena;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.ValueLayout;
import java.util.ArrayList;
import java.util.List;

import hdf.hdf5lib.structs.H5O_token_t;

import org.hdfgroup.javahdf5.*;

public class H5Ex_G_Traverse {

    private static String FILENAME = "groups/h5ex_g_traverse.h5";

    // Operator data structure for iteration
    static class OpData {
        int recurs;            // Recursion level
        OpData prev;           // Previous operator data
        H5O_token_t obj_token; // Object token
    }

    private static void OpenGroup(Arena arena)
    {
        long file_id = H5I_INVALID_HID();

        try {
            // Open file
            file_id = H5Fopen(arena.allocateFrom(FILENAME), H5F_ACC_RDONLY(), H5P_DEFAULT());

            if (file_id >= 0) {
                // Get info for root group
                MemorySegment root_info = arena.allocate(H5O_info2_t.sizeof());
                H5Oget_info3(file_id, root_info, H5O_INFO_ALL());

                // Initialize operator data
                OpData od    = new OpData();
                od.recurs    = 0;
                od.prev      = null;
                od.obj_token = new H5O_token_t(H5O_info2_t.token(root_info));

                // Print root group and begin iteration
                System.out.println("/ {");

                // Create and iterate
                iterateGroup(arena, file_id, od);

                System.out.println("}");
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }
        finally {
            // Close and release resources
            try {
                if (file_id >= 0)
                    H5Fclose(file_id);
            }
            catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    private static void iterateGroup(Arena arena, long group_id, OpData od)
    {
        // Create callback for H5Literate
        H5L_iterate2_t.Function link_callback =
            (long group, MemorySegment name, MemorySegment info, MemorySegment op_data) ->
        {
            String link_name = name.getString(0);
            int spaces       = 2 * (od.recurs + 1); // Indentation

            try {
                // Get object info
                MemorySegment obj_info = arena.allocate(H5O_info2_t.sizeof());
                int ret = H5Oget_info_by_name3(group, name, obj_info, H5O_INFO_ALL(), H5P_DEFAULT());

                if (ret >= 0) {
                    int obj_type          = H5O_info2_t.type(obj_info);
                    H5O_token_t obj_token = new H5O_token_t(H5O_info2_t.token(obj_info));

                    // Print indentation
                    for (int i = 0; i < spaces; i++)
                        System.out.print(" ");

                    if (obj_type == H5O_TYPE_GROUP()) {
                        System.out.println("Group: " + link_name + " {");

                        // Check for loops
                        if (groupCheck(od, obj_token)) {
                            for (int i = 0; i < spaces; i++)
                                System.out.print(" ");
                            System.out.println("  Warning: Loop detected!");
                        }
                        else {
                            // Create new operator data for recursion
                            OpData nextod    = new OpData();
                            nextod.recurs    = od.recurs + 1;
                            nextod.prev      = od;
                            nextod.obj_token = obj_token;

                            // Recurse into group
                            long subgroup_id = H5Gopen2(group, name, H5P_DEFAULT());
                            if (subgroup_id >= 0) {
                                try {
                                    iterateGroup(arena, subgroup_id, nextod);
                                }
                                finally {
                                    H5Gclose(subgroup_id);
                                }
                            }
                        }

                        for (int i = 0; i < spaces; i++)
                            System.out.print(" ");
                        System.out.println("}");
                    }
                    else if (obj_type == H5O_TYPE_DATASET()) {
                        System.out.println("Dataset: " + link_name);
                    }
                    else if (obj_type == H5O_TYPE_NAMED_DATATYPE()) {
                        System.out.println("Datatype: " + link_name);
                    }
                    else {
                        System.out.println("Unknown: " + link_name);
                    }
                }
            }
            catch (Exception e) {
                e.printStackTrace();
            }

            return 0; // Continue iteration
        };

        // Allocate upcall stub
        MemorySegment link_callback_stub = H5L_iterate2_t.allocate(link_callback, arena);

        // Iterate over links in group
        H5Literate2(group_id, H5_INDEX_NAME(), H5_ITER_NATIVE(), MemorySegment.NULL, link_callback_stub,
                    MemorySegment.NULL);
    }

    // Check if we've already visited this object token (loop detection)
    private static boolean groupCheck(OpData od, H5O_token_t target_token)
    {
        if (od.obj_token.equals(target_token))
            return true; // Object tokens match
        else if (od.recurs == 0)
            return false; // Root group reached with no matches
        else
            return groupCheck(od.prev, target_token); // Recursively examine the next node
    }

    public static void main(String[] args)
    {
        try (Arena arena = Arena.ofConfined()) {
            H5Ex_G_Traverse.OpenGroup(arena);
        }
    }
}
