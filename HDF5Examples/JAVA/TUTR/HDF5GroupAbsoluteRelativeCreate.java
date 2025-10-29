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
   Creating groups using absolute and relative names.
 ************************************************************/

import static org.hdfgroup.javahdf5.hdf5_h.*;

import java.lang.foreign.Arena;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.ValueLayout;

public class HDF5GroupAbsoluteRelativeCreate {
    private static String FILENAME    = "HDF5GroupAbsoluteRelativeCreate.h5";
    private static String GROUPNAME   = "MyGroup";
    private static String GROUPNAME_A = "GroupA";
    private static String GROUPNAME_B = "GroupB";

    private static void CreateGroupAbsoluteAndRelative(Arena arena)
    {
        long file_id   = H5I_INVALID_HID();
        long group1_id = H5I_INVALID_HID();
        long group2_id = H5I_INVALID_HID();
        long group3_id = H5I_INVALID_HID();

        // Create a new file using default properties.
        try {
            file_id = H5Fcreate(arena.allocateFrom(FILENAME), H5F_ACC_TRUNC(), H5P_DEFAULT(), H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create a group named "/MyGroup" in the file.
        try {
            if (file_id >= 0)
                group1_id = H5Gcreate2(file_id, arena.allocateFrom("/" + GROUPNAME), H5P_DEFAULT(),
                                       H5P_DEFAULT(), H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create group "Group_A" in group "MyGroup" using absolute name.
        try {
            if (file_id >= 0)
                group2_id = H5Gcreate2(file_id, arena.allocateFrom("/" + GROUPNAME + "/" + GROUPNAME_A),
                                       H5P_DEFAULT(), H5P_DEFAULT(), H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create group "Group_B" in group "MyGroup" using relative name.
        try {
            if (group1_id >= 0)
                group3_id = H5Gcreate2(group1_id, arena.allocateFrom(GROUPNAME_B), H5P_DEFAULT(),
                                       H5P_DEFAULT(), H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Close the group3.
        try {
            if (group3_id >= 0)
                H5Gclose(group3_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Close the group2.
        try {
            if (group2_id >= 0)
                H5Gclose(group2_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Close the group1.
        try {
            if (group1_id >= 0)
                H5Gclose(group1_id);
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
            HDF5GroupAbsoluteRelativeCreate.CreateGroupAbsoluteAndRelative(arena);
        }
    }
}
