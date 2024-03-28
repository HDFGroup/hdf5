/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;
import hdf.hdf5lib.structs.H5G_info_t;
import hdf.hdf5lib.structs.H5O_token_t;

/**
 * <p>
 * Title: HDF Native Package (Java) Example
 * </p>
 * <p>
 * Description: this example shows how to retrieve HDF file structure using the
 * "HDF Native Package (Java)". The example created the group structure and
 * datasets, and print out the file structure:
 *
 * <pre>
 *     "/" (root)
 *         integer arrays
 *             2D 32-bit integer 20x10
 *             3D unsigned 8-bit integer 20x10x5
 *         float arrays
 *             2D 64-bit double 20x10
 *             3D 32-bit float  20x10x5
 * </pre>
 *
 * </p>
 */
public class HDF5FileStructure {
    private static String fname = "HDF5FileStructure.h5";

    private static void FileStructure()
    {
        long file_id  = HDF5Constants.H5I_INVALID_HID;
        long group_id = HDF5Constants.H5I_INVALID_HID;

        // create the file and add groups and dataset into the file
        try {
            createFile();
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Open file using the default properties.
        try {
            file_id = H5.H5Fopen(fname, HDF5Constants.H5F_ACC_RDWR, HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Open the group, obtaining a new handle.
        try {
            if (file_id >= 0)
                group_id = H5.H5Gopen(file_id, "/", HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        try {
            printGroup(group_id, "/", "");
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

    /**
     * Recursively print a group and its members.
     *
     * @throws Exception
     */
    private static void printGroup(long g_id, String gname, String indent) throws Exception
    {
        if (g_id < 0)
            return;

        H5G_info_t members      = H5.H5Gget_info(g_id);
        String objNames[]       = new String[(int)members.nlinks];
        int objTypes[]          = new int[(int)members.nlinks];
        int lnkTypes[]          = new int[(int)members.nlinks];
        H5O_token_t objTokens[] = new H5O_token_t[(int)members.nlinks];
        int names_found         = 0;
        try {
            names_found = H5.H5Gget_obj_info_all(g_id, null, objNames, objTypes, lnkTypes, objTokens,
                                                 HDF5Constants.H5_INDEX_NAME);
        }
        catch (Throwable err) {
            err.printStackTrace();
        }

        indent += "    ";
        for (int i = 0; i < names_found; i++) {
            System.out.println(indent + objNames[i]);
            long group_id = HDF5Constants.H5I_INVALID_HID;
            if (objTypes[i] == HDF5Constants.H5O_TYPE_GROUP) {
                // Open the group, obtaining a new handle.
                try {
                    if (g_id >= 0)
                        group_id = H5.H5Gopen(g_id, objNames[i], HDF5Constants.H5P_DEFAULT);
                }
                catch (Exception e) {
                    e.printStackTrace();
                }

                if (group_id >= 0)
                    printGroup(group_id, objNames[i], indent);

                // Close the group.
                try {
                    if (group_id >= 0)
                        H5.H5Gclose(group_id);
                }
                catch (Exception e) {
                    e.printStackTrace();
                }
            }
        }
    }

    /**
     * create the file and add groups ans dataset into the file, which is the
     * same as javaExample.H5DatasetCreate
     *
     * @see javaExample.HDF5DatasetCreate
     * @throws Exception
     */
    private static void createFile() throws Exception
    {
        long[] dims2D      = {20, 10};
        long[] dims3D      = {20, 10, 5};
        long file_id       = HDF5Constants.H5I_INVALID_HID;
        long dataset_id    = HDF5Constants.H5I_INVALID_HID;
        long dataspace_id1 = HDF5Constants.H5I_INVALID_HID;
        long dataspace_id2 = HDF5Constants.H5I_INVALID_HID;
        long group_id1     = HDF5Constants.H5I_INVALID_HID;
        long group_id2     = HDF5Constants.H5I_INVALID_HID;

        // Create a new file using default properties.
        try {
            file_id = H5.H5Fcreate(fname, HDF5Constants.H5F_ACC_TRUNC, HDF5Constants.H5P_DEFAULT,
                                   HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create groups in the file.
        try {
            if (file_id >= 0) {
                group_id1 = H5.H5Gcreate(file_id,
                                         "/"
                                             + "integer arrays",
                                         HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT,
                                         HDF5Constants.H5P_DEFAULT);
                group_id1 = H5.H5Gcreate(file_id,
                                         "/"
                                             + "float arrays",
                                         HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT,
                                         HDF5Constants.H5P_DEFAULT);
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create the data space for the datasets.
        try {
            dataspace_id1 = H5.H5Screate_simple(2, dims2D, null);
            dataspace_id2 = H5.H5Screate_simple(3, dims3D, null);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // create 2D 32-bit (4 bytes) integer dataset of 20 by 10
        try {
            if ((file_id >= 0) && (dataspace_id1 >= 0))
                dataset_id =
                    H5.H5Dcreate(file_id,
                                 "/"
                                     + "integer arrays"
                                     + "/"
                                     + "2D 32-bit integer 20x10",
                                 HDF5Constants.H5T_STD_I32LE, dataspace_id1, HDF5Constants.H5P_DEFAULT,
                                 HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Close the dataset.
        try {
            if (dataset_id >= 0)
                H5.H5Dclose(dataset_id);
            dataset_id = HDF5Constants.H5I_INVALID_HID;
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // create 3D 8-bit (1 byte) unsigned integer dataset of 20 by 10 by 5
        try {
            if ((file_id >= 0) && (dataspace_id2 >= 0))
                dataset_id =
                    H5.H5Dcreate(file_id,
                                 "/"
                                     + "integer arrays"
                                     + "/"
                                     + "3D 8-bit unsigned integer 20x10x5",
                                 HDF5Constants.H5T_STD_I64LE, dataspace_id2, HDF5Constants.H5P_DEFAULT,
                                 HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Close the dataset.
        try {
            if (dataset_id >= 0)
                H5.H5Dclose(dataset_id);
            dataset_id = HDF5Constants.H5I_INVALID_HID;
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // create 2D 64-bit (8 bytes) double dataset of 20 by 10
        try {
            if ((file_id >= 0) && (dataspace_id1 >= 0))
                dataset_id =
                    H5.H5Dcreate(file_id,
                                 "/"
                                     + "float arrays"
                                     + "/"
                                     + "2D 64-bit double 20x10",
                                 HDF5Constants.H5T_NATIVE_DOUBLE, dataspace_id1, HDF5Constants.H5P_DEFAULT,
                                 HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Close the dataset.
        try {
            if (dataset_id >= 0)
                H5.H5Dclose(dataset_id);
            dataset_id = HDF5Constants.H5I_INVALID_HID;
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // create 3D 32-bit (4 bytes) float dataset of 20 by 10 by 5
        try {
            if ((file_id >= 0) && (dataspace_id2 >= 0))
                dataset_id =
                    H5.H5Dcreate(file_id,
                                 "/"
                                     + "float arrays"
                                     + "/"
                                     + "3D 32-bit float  20x10x5",
                                 HDF5Constants.H5T_NATIVE_FLOAT, dataspace_id2, HDF5Constants.H5P_DEFAULT,
                                 HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Close the dataset.
        try {
            if (dataset_id >= 0)
                H5.H5Dclose(dataset_id);
            dataset_id = HDF5Constants.H5I_INVALID_HID;
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Close the data space.
        try {
            if (dataspace_id1 >= 0)
                H5.H5Sclose(dataspace_id1);
            dataspace_id1 = HDF5Constants.H5I_INVALID_HID;
            if (dataspace_id2 >= 0)
                H5.H5Sclose(dataspace_id2);
            dataspace_id2 = HDF5Constants.H5I_INVALID_HID;
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Close the groups.
        try {
            if (group_id1 >= 0)
                H5.H5Gclose(group_id1);
            if (group_id2 >= 0)
                H5.H5Gclose(group_id2);
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

    public static void main(String[] args) { HDF5FileStructure.FileStructure(); }
}
