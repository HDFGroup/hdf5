/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/************************************************************
   Creating groups using absolute and relative names.
 ************************************************************/

package examples.intro;

import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;

public class H5_CreateGroupAbsoluteRelative {
    private static String FILENAME = "H5_CreateGroupAbsoluteRelative.h5";
    private static String GROUPNAME = "MyGroup";
    private static String GROUPNAME_A = "GroupA";
    private static String GROUPNAME_B = "GroupB";

    private static void CreateGroupAbsoluteAndRelative() {
        long file_id = -1;
        long group1_id = -1;
        long group2_id = -1;
        long group3_id = -1;

        // Create a new file using default properties.
        try {
            file_id = H5.H5Fcreate(FILENAME, HDF5Constants.H5F_ACC_TRUNC, HDF5Constants.H5P_DEFAULT,
                    HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create a group named "/MyGroup" in the file.
        try {
            if (file_id >= 0)
                group1_id = H5.H5Gcreate(file_id, "/" + GROUPNAME, HDF5Constants.H5P_DEFAULT,
                        HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create group "Group_A" in group "MyGroup" using absolute name.
        try {
            if (file_id >= 0)
                group2_id = H5.H5Gcreate(file_id, "/" + GROUPNAME + "/" + GROUPNAME_A, HDF5Constants.H5P_DEFAULT,
                        HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create group "Group_B" in group "MyGroup" using relative name.
        try {
            if (group1_id >= 0)
                group3_id = H5.H5Gcreate(group1_id, GROUPNAME_B, HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT,
                        HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Close the group3.
        try {
            if (group3_id >= 0)
                H5.H5Gclose(group3_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Close the group2.
        try {
            if (group2_id >= 0)
                H5.H5Gclose(group2_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Close the group1.
        try {
            if (group1_id >= 0)
                H5.H5Gclose(group1_id);
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

    public static void main(String[] args) {
        H5_CreateGroupAbsoluteRelative.CreateGroupAbsoluteAndRelative();
    }

}
