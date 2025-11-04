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
 This example shows how to create intermediate groups with
 a single call to H5Gcreate.
 ************************************************************/

import static org.hdfgroup.javahdf5.hdf5_h.*;

import java.lang.foreign.Arena;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.ValueLayout;

import org.hdfgroup.javahdf5.*;

public class H5Ex_G_Intermediate {

    private static String FILENAME = "H5Ex_G_Intermediate.h5";

    private void CreateGroup(Arena arena) throws Exception
    {
        long file_id  = H5I_INVALID_HID();
        long group_id = H5I_INVALID_HID();
        long gcpl_id  = H5I_INVALID_HID();

        try {
            // Create a new file using the default properties
            file_id = H5Fcreate(arena.allocateFrom(FILENAME), H5F_ACC_TRUNC(), H5P_DEFAULT(), H5P_DEFAULT());

            // Create group creation property list and set it to allow creation of intermediate groups
            gcpl_id = H5Pcreate(H5P_CLS_LINK_CREATE_ID_g());
            H5Pset_create_intermediate_group(gcpl_id, 1); // 1 = true

            /*
             * Create the group /G1/G2/G3. Note that /G1 and /G1/G2 do not exist yet.
             * This call would cause an error if we did not use the previously created property list.
             */
            group_id =
                H5Gcreate2(file_id, arena.allocateFrom("/G1/G2/G3"), gcpl_id, H5P_DEFAULT(), H5P_DEFAULT());

            // Print all the objects in the file to show that intermediate groups have been created
            System.out.println("Objects in the file:");

            // Create callback for H5Ovisit
            H5O_iterate2_t.Function obj_callback =
                (long obj, MemorySegment name, MemorySegment info, MemorySegment op_data) ->
            {
                String obj_name = name.getString(0);
                int obj_type    = H5O_info2_t.type(info);

                System.out.print("/"); // Print root group in object path

                // Check if the current object is the root group, and if not print the full path name and type
                if (obj_name.charAt(0) == '.') {
                    // Root group, do not print '.'
                    System.out.println("  (Group)");
                }
                else if (obj_type == H5O_TYPE_GROUP()) {
                    System.out.println(obj_name + "  (Group)");
                }
                else if (obj_type == H5O_TYPE_DATASET()) {
                    System.out.println(obj_name + "  (Dataset)");
                }
                else if (obj_type == H5O_TYPE_NAMED_DATATYPE()) {
                    System.out.println(obj_name + "  (Datatype)");
                }
                else {
                    System.out.println(obj_name + "  (Unknown)");
                }

                return 0; // Continue iteration
            };

            // Allocate upcall stub for callback
            MemorySegment obj_callback_stub = H5O_iterate2_t.allocate(obj_callback, arena);

            // Call H5Ovisit
            H5Ovisit3(file_id, H5_INDEX_NAME(), H5_ITER_NATIVE(), obj_callback_stub, MemorySegment.NULL,
                      H5O_INFO_ALL());
        }
        finally {
            // Close and release resources
            if (gcpl_id >= 0)
                H5Pclose(gcpl_id);
            if (group_id >= 0)
                H5Gclose(group_id);
            if (file_id >= 0)
                H5Fclose(file_id);
        }
    }

    public static void main(String[] args)
    {
        try (Arena arena = Arena.ofConfined()) {
            (new H5Ex_G_Intermediate()).CreateGroup(arena);
        }
        catch (Exception ex) {
            ex.printStackTrace();
        }
    }
}
