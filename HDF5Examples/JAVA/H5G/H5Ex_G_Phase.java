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
  This example shows how to set the conditions for
  conversion between compact and dense (indexed) groups.
 ************************************************************/

import static org.hdfgroup.javahdf5.hdf5_h.*;

import java.lang.foreign.Arena;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.ValueLayout;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;

import org.hdfgroup.javahdf5.H5G_info_t;

public class H5Ex_G_Phase {
    private static String FILENAME = "H5Ex_G_Phase.h5";
    private static int MAX_GROUPS  = 7;
    private static int MAX_COMPACT = 5;
    private static int MIN_DENSE   = 3;

    enum H5G_storage {
        H5G_STORAGE_TYPE_UNKNOWN(-1),
        H5G_STORAGE_TYPE_SYMBOL_TABLE(0),
        H5G_STORAGE_TYPE_COMPACT(1),
        H5G_STORAGE_TYPE_DENSE(2);

        private static final Map<Integer, H5G_storage> lookup = new HashMap<Integer, H5G_storage>();

        static
        {
            for (H5G_storage s : EnumSet.allOf(H5G_storage.class))
                lookup.put(s.getCode(), s);
        }

        private int code;

        H5G_storage(int layout_type) { this.code = layout_type; }

        public int getCode() { return this.code; }

        public static H5G_storage get(int code) { return lookup.get(code); }
    }

    private static void CreateGroup(Arena arena)
    {
        long file_id        = H5I_INVALID_HID();
        long group_id       = H5I_INVALID_HID();
        long subgroup_id    = H5I_INVALID_HID();
        long fapl_id        = H5I_INVALID_HID();
        long gcpl_id        = H5I_INVALID_HID();
        MemorySegment ginfo = H5G_info_t.allocate(arena);
        String name         = "G0"; // Name of subgroup_id
        int i;

        // Set file access property list to allow the latest file format.This will allow the library to create
        // new format groups.
        try {
            fapl_id = H5Pcreate(H5P_CLS_FILE_ACCESS_ID_g());
            if (fapl_id >= 0)
                H5Pset_libver_bounds(fapl_id, H5F_LIBVER_LATEST(), H5F_LIBVER_LATEST());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create group access property list and set the phase change conditions.
        try {
            gcpl_id = H5Pcreate(H5P_CLS_GROUP_CREATE_ID_g());
            if (gcpl_id >= 0)
                H5Pset_link_phase_change(gcpl_id, MAX_COMPACT, MIN_DENSE);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create a new file using the default properties.
        try {
            if (fapl_id >= 0)
                file_id = H5Fcreate(arena.allocateFrom(FILENAME), H5F_ACC_TRUNC(), H5P_DEFAULT(), fapl_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create primary group.
        try {
            if ((file_id >= 0) && (gcpl_id >= 0))
                group_id =
                    H5Gcreate2(file_id, arena.allocateFrom(name), H5P_DEFAULT(), gcpl_id, H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Add subgroups to "group" one at a time, print the storage type for "group" after each subgroup is
        // created.
        for (i = 1; i <= MAX_GROUPS; i++) {
            // Define the subgroup name and create the subgroup.
            char append = (char)(((char)i) + '0');
            name        = name + append; /* G1, G2, G3 etc. */
            try {
                if (group_id >= 0) {
                    subgroup_id = H5Gcreate2(group_id, arena.allocateFrom(name), H5P_DEFAULT(), H5P_DEFAULT(),
                                             H5P_DEFAULT());
                    H5Gclose(subgroup_id);
                }
            }
            catch (Exception e) {
                e.printStackTrace();
            }

            // Obtain the group info and print the group storage type
            try {
                if (group_id >= 0) {
                    H5Gget_info(group_id, ginfo);
                    System.out.print(H5G_info_t.nlinks(ginfo) + " Group" +
                                     (H5G_info_t.nlinks(ginfo) == 1 ? " " : "s") + ": Storage type is ");
                    switch (H5G_storage.get(H5G_info_t.storage_type(ginfo))) {
                    case H5G_STORAGE_TYPE_COMPACT:
                        System.out.println("H5G_STORAGE_TYPE_COMPACT"); // New compact format
                        break;
                    case H5G_STORAGE_TYPE_DENSE:
                        System.out.println("H5G_STORAGE_TYPE_DENSE"); // New dense (indexed) format
                        break;
                    case H5G_STORAGE_TYPE_SYMBOL_TABLE:
                        System.out.println("H5G_STORAGE_TYPE_SYMBOL_TABLE"); // Original format
                        break;
                    case H5G_STORAGE_TYPE_UNKNOWN:
                        System.out.println("H5G_STORAGE_TYPE_UNKNOWN");
                        break;
                    default:
                        System.out.println("Storage Type Invalid");
                        break;
                    }
                }
            }
            catch (Exception e) {
                e.printStackTrace();
            }
        }

        System.out.println();

        // Delete subgroups one at a time, print the storage type for "group" after each subgroup is deleted.
        for (i = MAX_GROUPS; i >= 1; i--) {
            // Define the subgroup name and delete the subgroup.
            try {
                H5Ldelete(group_id, arena.allocateFrom(name), H5P_DEFAULT());
            }
            catch (Exception e) {
                e.printStackTrace();
            }
            name = name.substring(0, i + 1);

            // Obtain the group info and print the group storage type
            try {
                if (group_id >= 0) {
                    H5Gget_info(group_id, ginfo);
                    System.out.print(H5G_info_t.nlinks(ginfo) + " Group" +
                                     (H5G_info_t.nlinks(ginfo) == 1 ? " " : "s") + ": Storage type is ");
                    switch (H5G_storage.get(H5G_info_t.storage_type(ginfo))) {
                    case H5G_STORAGE_TYPE_COMPACT:
                        System.out.println("H5G_STORAGE_TYPE_COMPACT"); // New compact format
                        break;
                    case H5G_STORAGE_TYPE_DENSE:
                        System.out.println("H5G_STORAGE_TYPE_DENSE"); // New dense (indexed) format
                        break;
                    case H5G_STORAGE_TYPE_SYMBOL_TABLE:
                        System.out.println("H5G_STORAGE_TYPE_SYMBOL_TABLE"); // Original format
                        break;
                    case H5G_STORAGE_TYPE_UNKNOWN:
                        System.out.println("H5G_STORAGE_TYPE_UNKNOWN");
                        break;
                    default:
                        System.out.println("Storage Type Invalid");
                        break;
                    }
                }
            }
            catch (Exception e) {
                e.printStackTrace();
            }
        }

        // Close and release resources
        try {
            if (fapl_id >= 0)
                H5Pclose(fapl_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        try {
            if (gcpl_id >= 0)
                H5Pclose(gcpl_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Close the group
        try {
            if (group_id >= 0)
                H5Gclose(group_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Close the file
        try {
            if (file_id >= 0)
                H5Fclose(file_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static void main(String[] args)
    {
        try (Arena arena = Arena.ofConfined()) {
            H5Ex_G_Phase.CreateGroup(arena);
        }
    }
}
