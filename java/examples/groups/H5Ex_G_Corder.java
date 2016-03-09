/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/************************************************************
    Creating a file with creation properties and traverse the
    groups in alpabetical and creation order.
 ************************************************************/

package examples.groups;

import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;
import hdf.hdf5lib.structs.H5G_info_t;

public class H5Ex_G_Corder {
    private static String FILE = "H5Ex_G_Corder.h5";

    private static void CreateGroup() throws Exception {
        long file_id = -1;
        long group_id = -1;
        long subgroup_id = -1;
        long gcpl_id = -1;
        int status;
        H5G_info_t ginfo;
        int i;
        String name;

        try {
            // Create a new file using default properties.
            file_id = H5.H5Fcreate(FILE, HDF5Constants.H5F_ACC_TRUNC, HDF5Constants.H5P_DEFAULT,
                    HDF5Constants.H5P_DEFAULT);

            // Create group creation property list and enable link creation order tracking.
            gcpl_id = H5.H5Pcreate(HDF5Constants.H5P_GROUP_CREATE);
            status = H5.H5Pset_link_creation_order(gcpl_id, HDF5Constants.H5P_CRT_ORDER_TRACKED
                    + HDF5Constants.H5P_CRT_ORDER_INDEXED);

            // Create primary group using the property list.
            if (status >= 0)
                group_id = H5.H5Gcreate(file_id, "index_group", HDF5Constants.H5P_DEFAULT, gcpl_id,
                        HDF5Constants.H5P_DEFAULT);

            try {
                /*
                 * Create subgroups in the primary group. These will be tracked by creation order. Note that these
                 * groups do not have to have the creation order tracking property set.
                 */
                subgroup_id = H5.H5Gcreate(group_id, "H", HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT,
                        HDF5Constants.H5P_DEFAULT);
                status = H5.H5Gclose(subgroup_id);
                subgroup_id = H5.H5Gcreate(group_id, "D", HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT,
                        HDF5Constants.H5P_DEFAULT);
                status = H5.H5Gclose(subgroup_id);
                subgroup_id = H5.H5Gcreate(group_id, "F", HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT,
                        HDF5Constants.H5P_DEFAULT);
                status = H5.H5Gclose(subgroup_id);
                subgroup_id = H5.H5Gcreate(group_id, "5", HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT,
                        HDF5Constants.H5P_DEFAULT);
                status = H5.H5Gclose(subgroup_id);

                // Get group info.
                ginfo = H5.H5Gget_info(group_id);

                // Traverse links in the primary group using alphabetical indices (H5_INDEX_NAME).
                System.out.println("Traversing group using alphabetical indices:");
                for (i = 0; i < ginfo.nlinks; i++) {
                    // Retrieve the name of the ith link in a group
                    name = H5.H5Lget_name_by_idx(group_id, ".", HDF5Constants.H5_INDEX_NAME, HDF5Constants.H5_ITER_INC,
                            i, HDF5Constants.H5P_DEFAULT);
                    System.out.println("Index " + i + ": " + name);
                }

                // Traverse links in the primary group by creation order (H5_INDEX_CRT_ORDER).
                System.out.println("Traversing group using creation order indices:");
                for (i = 0; i < ginfo.nlinks; i++) {
                    // Retrieve the name of the ith link in a group
                    name = H5.H5Lget_name_by_idx(group_id, ".", HDF5Constants.H5_INDEX_CRT_ORDER,
                            HDF5Constants.H5_ITER_INC, i, HDF5Constants.H5P_DEFAULT);
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
                H5.H5Pclose(gcpl_id);
            if (group_id >= 0)
                H5.H5Gclose(group_id);
            if (file_id >= 0)
                H5.H5Fclose(file_id);
        }
    }

    public static void main(String[] args) {
        try {
            H5Ex_G_Corder.CreateGroup();
        }
        catch (Exception ex) {
            ex.printStackTrace();
        }
    }

}
