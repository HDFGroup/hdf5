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
  This example shows how to create, open, and close a group.
 ************************************************************/

import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;

public class H5Ex_G_Create {
    private static String FILENAME  = "H5Ex_G_Create.h5";
    private static String GROUPNAME = "G1";

    private static void CreateGroup()
    {
        long file_id  = HDF5Constants.H5I_INVALID_HID;
        long group_id = HDF5Constants.H5I_INVALID_HID;

        // Create a new file using default properties.
        try {
            file_id = H5.H5Fcreate(FILENAME, HDF5Constants.H5F_ACC_TRUNC, HDF5Constants.H5P_DEFAULT,
                                   HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create a group in the file.
        try {
            if (file_id >= 0)
                group_id = H5.H5Gcreate(file_id, "/" + GROUPNAME, HDF5Constants.H5P_DEFAULT,
                                        HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Close the group. The handle "group" can no longer be used.
        try {
            if (group_id >= 0)
                H5.H5Gclose(group_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Re-open the group, obtaining a new handle.
        try {
            if (file_id >= 0)
                group_id = H5.H5Gopen(file_id, "/" + GROUPNAME, HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Close the group.
        try {
            if (group_id >= 0)
                H5.H5Gclose(group_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Close the file.
        try {
            if (file_id >= 0)
                H5.H5Fclose(file_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static void main(String[] args) { H5Ex_G_Create.CreateGroup(); }
}
