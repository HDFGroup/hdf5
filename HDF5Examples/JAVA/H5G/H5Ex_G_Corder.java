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
    Creating a file with creation properties and traverse the
    groups in alphabetical and creation order.
 ************************************************************/

import static org.hdfgroup.javahdf5.hdf5_h.*;

import java.lang.foreign.Arena;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.ValueLayout;

import org.hdfgroup.javahdf5.H5G_info_t;

public class H5Ex_G_Corder {
    private static String FILENAME = "H5Ex_G_Corder.h5";

    private static void CreateGroup(Arena arena) throws Exception
    {
        long file_id     = H5I_INVALID_HID();
        long group_id    = H5I_INVALID_HID();
        long subgroup_id = H5I_INVALID_HID();
        long gcpl_id     = H5I_INVALID_HID();
        int status;
        MemorySegment ginfo = H5G_info_t.allocate(arena);
        int i;
        String name;

        try {
            // Create a new file using default properties.
            file_id = H5Fcreate(arena.allocateFrom(FILENAME), H5F_ACC_TRUNC(), H5P_DEFAULT(), H5P_DEFAULT());

            // Create group creation property list and enable link creation order tracking.
            gcpl_id = H5Pcreate(H5P_CLS_GROUP_CREATE_ID_g());
            status  = H5Pset_link_creation_order(gcpl_id, H5P_CRT_ORDER_TRACKED() + H5P_CRT_ORDER_INDEXED());

            // Create primary group using the property list.
            if (status >= 0)
                group_id = H5Gcreate2(file_id, arena.allocateFrom("index_group"), H5P_DEFAULT(), gcpl_id,
                                      H5P_DEFAULT());

            try {
                /*
                 * Create subgroups in the primary group. These will be tracked by creation order. Note that
                 * these groups do not have to have the creation order tracking property set.
                 */
                subgroup_id = H5Gcreate2(group_id, arena.allocateFrom("H"), H5P_DEFAULT(), H5P_DEFAULT(),
                                         H5P_DEFAULT());
                status      = H5Gclose(subgroup_id);
                subgroup_id = H5Gcreate2(group_id, arena.allocateFrom("D"), H5P_DEFAULT(), H5P_DEFAULT(),
                                         H5P_DEFAULT());
                status      = H5Gclose(subgroup_id);
                subgroup_id = H5Gcreate2(group_id, arena.allocateFrom("F"), H5P_DEFAULT(), H5P_DEFAULT(),
                                         H5P_DEFAULT());
                status      = H5Gclose(subgroup_id);
                subgroup_id = H5Gcreate2(group_id, arena.allocateFrom("5"), H5P_DEFAULT(), H5P_DEFAULT(),
                                         H5P_DEFAULT());
                status      = H5Gclose(subgroup_id);

                // Get group info.
                H5Gget_info(group_id, ginfo);
                long nlinks = H5G_info_t.nlinks(ginfo);

                // Traverse links in the primary group using alphabetical indices (H5_INDEX_NAME).
                System.out.println("Traversing group using alphabetical indices:");
                for (i = 0; i < nlinks; i++) {
                    // Retrieve the name of the ith link in a group - first query size
                    long name_size =
                        H5Lget_name_by_idx(group_id, arena.allocateFrom("."), H5_INDEX_NAME(), H5_ITER_INC(),
                                           i, MemorySegment.NULL, 0, H5P_DEFAULT());
                    MemorySegment nameBuffer = arena.allocate(name_size + 1);
                    H5Lget_name_by_idx(group_id, arena.allocateFrom("."), H5_INDEX_NAME(), H5_ITER_INC(), i,
                                       nameBuffer, name_size + 1, H5P_DEFAULT());
                    name = nameBuffer.getString(0);
                    System.out.println("Index " + i + ": " + name);
                }

                // Traverse links in the primary group by creation order (H5_INDEX_CRT_ORDER).
                System.out.println("Traversing group using creation order indices:");
                for (i = 0; i < nlinks; i++) {
                    // Retrieve the name of the ith link in a group - first query size
                    long name_size =
                        H5Lget_name_by_idx(group_id, arena.allocateFrom("."), H5_INDEX_CRT_ORDER(),
                                           H5_ITER_INC(), i, MemorySegment.NULL, 0, H5P_DEFAULT());
                    MemorySegment nameBuffer = arena.allocate(name_size + 1);
                    H5Lget_name_by_idx(group_id, arena.allocateFrom("."), H5_INDEX_CRT_ORDER(), H5_ITER_INC(),
                                       i, nameBuffer, name_size + 1, H5P_DEFAULT());
                    name = nameBuffer.getString(0);
                    System.out.println("Index " + i + ": " + name);
                }
            }
            catch (Exception e) {
                e.printStackTrace();
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }
        finally {
            // Close and release resources.
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
            try {
                H5Ex_G_Corder.CreateGroup(arena);
            }
            catch (Exception ex) {
                ex.printStackTrace();
            }
        }
    }
}
