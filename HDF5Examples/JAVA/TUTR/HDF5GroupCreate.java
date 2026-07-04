/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the LICENSE file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

import static org.hdfgroup.javahdf5.hdf5_h.*;

import java.lang.foreign.Arena;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.ValueLayout;

/**
 * <p>
 * Title: HDF Native Package (Java) Example
 * </p>
 * <p>
 * Description: this example shows how to create HDF5 groups using the
 * "HDF Native Package (Java)". The example created the group structure:
 *
 * <pre>
 *     "/" (root)
 *         g1
 *             g11
 *             g12
 *         g2
 *             g21
 *             g22
 * </pre>
 *
 * </p>
 */
public class HDF5GroupCreate {
    private static String fname = "HDF5GroupCreate.h5";

    private static void CreateGroup(Arena arena)
    {
        long file_id     = H5I_INVALID_HID();
        long subgroup_id = H5I_INVALID_HID();
        long group_id1   = H5I_INVALID_HID();
        long group_id2   = H5I_INVALID_HID();

        // Create a new file using default properties.
        try {
            file_id = H5Fcreate(arena.allocateFrom(fname), H5F_ACC_TRUNC(), H5P_DEFAULT(), H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
            System.err.println("Failed to create file:" + fname);
            return;
        }

        // Create a group in the file.
        try {
            if (file_id >= 0) {
                group_id1 = H5Gcreate2(file_id, arena.allocateFrom("g1"), H5P_DEFAULT(), H5P_DEFAULT(),
                                       H5P_DEFAULT());
                if (group_id1 >= 0) {
                    subgroup_id = H5Gcreate2(group_id1, arena.allocateFrom("g11"), H5P_DEFAULT(),
                                             H5P_DEFAULT(), H5P_DEFAULT());
                    try {
                        if (subgroup_id >= 0)
                            H5Gclose(subgroup_id);
                    }
                    catch (Exception e) {
                        e.printStackTrace();
                    }
                    subgroup_id = H5Gcreate2(group_id1, arena.allocateFrom("g12"), H5P_DEFAULT(),
                                             H5P_DEFAULT(), H5P_DEFAULT());
                    try {
                        if (subgroup_id >= 0)
                            H5Gclose(subgroup_id);
                    }
                    catch (Exception e) {
                        e.printStackTrace();
                    }
                }
                group_id2 = H5Gcreate2(file_id, arena.allocateFrom("g2"), H5P_DEFAULT(), H5P_DEFAULT(),
                                       H5P_DEFAULT());
                if (group_id2 >= 0) {
                    subgroup_id = H5Gcreate2(group_id2, arena.allocateFrom("g21"), H5P_DEFAULT(),
                                             H5P_DEFAULT(), H5P_DEFAULT());
                    try {
                        if (subgroup_id >= 0)
                            H5Gclose(subgroup_id);
                    }
                    catch (Exception e) {
                        e.printStackTrace();
                    }
                    subgroup_id = H5Gcreate2(group_id2, arena.allocateFrom("g22"), H5P_DEFAULT(),
                                             H5P_DEFAULT(), H5P_DEFAULT());
                    try {
                        if (subgroup_id >= 0)
                            H5Gclose(subgroup_id);
                    }
                    catch (Exception e) {
                        e.printStackTrace();
                    }
                }
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Close the groups.
        try {
            if (group_id2 >= 0)
                H5Gclose(group_id2);
        }
        catch (Exception e) {
            e.printStackTrace();
        }
        try {
            if (group_id1 >= 0)
                H5Gclose(group_id1);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Close the file.
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
            HDF5GroupCreate.CreateGroup(arena);
        }
    }
}
