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
 This example shows how to recursively traverse a file
 using H5Ovisit and H5Lvisit.  The program prints all of
 the objects in the file specified in FILE, then prints all
 of the links in that file.  The default file used by this
 example implements the structure described in the User
 Guide, chapter 4, figure 26.
 ************************************************************/

import static org.hdfgroup.javahdf5.hdf5_h.*;

import java.lang.foreign.Arena;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.ValueLayout;

import org.hdfgroup.javahdf5.*;

public class H5Ex_G_Visit {

    private static String FILENAME = "groups/h5ex_g_visit.h5";

    public static void main(String[] args)
    {
        try (Arena arena = Arena.ofConfined()) {
            (new H5Ex_G_Visit()).VisitGroup(arena);
        }
        catch (Exception ex) {
            ex.printStackTrace();
        }
    }

    private void VisitGroup(Arena arena) throws Exception
    {
        long file_id = H5I_INVALID_HID();

        try {
            // Open file
            file_id = H5Fopen(arena.allocateFrom(FILENAME), H5F_ACC_RDONLY(), H5P_DEFAULT());

            // Begin iteration using H5Ovisit
            System.out.println("Objects in the file:");

            // Create callback for H5Ovisit
            H5O_iterate2_t.Function obj_callback =
                (long group, MemorySegment name, MemorySegment info, MemorySegment op_data) ->
            {
                String obj_name = name.getString(0);
                int obj_type    = H5O_info2_t.type(info);

                System.out.print("/"); // Print root group in object path

                // Check if the current object is the root group
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

            // Allocate upcall stub for object callback
            MemorySegment obj_callback_stub = H5O_iterate2_t.allocate(obj_callback, arena);

            // Call H5Ovisit
            H5Ovisit3(file_id, H5_INDEX_NAME(), H5_ITER_NATIVE(), obj_callback_stub, MemorySegment.NULL,
                      H5O_INFO_ALL());

            System.out.println();

            // Repeat the same process using H5Lvisit
            System.out.println("Links in the file:");

            // Create callback for H5Lvisit
            H5L_iterate2_t.Function link_callback =
                (long group, MemorySegment name, MemorySegment info, MemorySegment op_data) ->
            {
                String link_name = name.getString(0);
                int link_type    = H5L_info2_t.type(info);

                // Get type of the object the link points to
                try {
                    MemorySegment obj_info = arena.allocate(H5O_info2_t.sizeof());
                    int ret = H5Oget_info_by_name3(group, name, obj_info, H5O_INFO_ALL(), H5P_DEFAULT());

                    if (ret >= 0) {
                        int obj_type = H5O_info2_t.type(obj_info);

                        System.out.print("/"); // Print root group in object path

                        // Check if current object is root group
                        if (link_name.charAt(0) == '.') {
                            System.out.println("  (Group)");
                        }
                        else if (obj_type == H5O_TYPE_GROUP()) {
                            System.out.println(link_name + "  (Group)");
                        }
                        else if (obj_type == H5O_TYPE_DATASET()) {
                            System.out.println(link_name + "  (Dataset)");
                        }
                        else if (obj_type == H5O_TYPE_NAMED_DATATYPE()) {
                            System.out.println(link_name + "  (Datatype)");
                        }
                        else {
                            System.out.println(link_name + "  (Unknown)");
                        }
                    }
                }
                catch (Exception e) {
                    e.printStackTrace();
                }

                return 0; // Continue iteration
            };

            // Allocate upcall stub for link callback
            MemorySegment link_callback_stub = H5L_iterate2_t.allocate(link_callback, arena);

            // Call H5Lvisit
            H5Lvisit2(file_id, H5_INDEX_NAME(), H5_ITER_NATIVE(), link_callback_stub, MemorySegment.NULL);
        }
        finally {
            // Close and release resources
            if (file_id >= 0)
                H5Fclose(file_id);
        }
    }
}
