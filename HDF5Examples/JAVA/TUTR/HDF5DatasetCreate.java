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
 * Description: this example shows how to create HDF5 datasets using the
 * "HDF Native Package (Java)". The example created the group structure and
 * datasets:
 *
 * <pre>
 *     "/" (root)
 *         integer arrays
 *             2D 32-bit integer 20x10
 *             3D 16-bit integer 20x10x5
 *         float arrays
 *             2D 64-bit double 20x10
 *             3D 32-bit float  20x10x5
 * </pre>
 *
 * </p>
 */
public class HDF5DatasetCreate {
    private static String fname  = "HDF5DatasetCreate.h5";
    private static long[] dims2D = {20, 10};
    private static long[] dims3D = {20, 10, 5};

    private static void CreateDataset(Arena arena)
    {
        long file_id       = H5I_INVALID_HID();
        long group_id1     = H5I_INVALID_HID();
        long group_id2     = H5I_INVALID_HID();
        long dataspace_id1 = H5I_INVALID_HID();
        long dataspace_id2 = H5I_INVALID_HID();
        long dataset_id    = H5I_INVALID_HID();

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
                group_id2 = H5Gcreate2(file_id, arena.allocateFrom("g2"), H5P_DEFAULT(), H5P_DEFAULT(),
                                       H5P_DEFAULT());
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create the data space for the  2D dataset.
        try {
            dataspace_id1 =
                H5Screate_simple(2, arena.allocateFrom(ValueLayout.JAVA_LONG, dims2D), MemorySegment.NULL);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create the data space for the  3D dataset.
        try {
            dataspace_id2 =
                H5Screate_simple(3, arena.allocateFrom(ValueLayout.JAVA_LONG, dims3D), MemorySegment.NULL);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // create 2D 32-bit (4 bytes) integer dataset of 20 by 10
        try {
            if ((group_id1 >= 0) && (dataspace_id1 >= 0)) {
                dataset_id =
                    H5Dcreate2(group_id1, arena.allocateFrom("2D 32-bit integer 20x10"), H5T_STD_I32LE_g(),
                               dataspace_id1, H5P_DEFAULT(), H5P_DEFAULT(), H5P_DEFAULT());
                if (dataset_id >= 0)
                    H5Dclose(dataset_id);
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // create 3D 8-bit (1 byte) unsigned integer dataset of 20 by 10 by 5
        try {
            if ((group_id1 >= 0) && (dataspace_id2 >= 0)) {
                dataset_id =
                    H5Dcreate2(group_id1, arena.allocateFrom("3D 8-bit unsigned integer 20x10x5"),
                               H5T_STD_U8LE_g(), dataspace_id2, H5P_DEFAULT(), H5P_DEFAULT(), H5P_DEFAULT());
                if (dataset_id >= 0)
                    H5Dclose(dataset_id);
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // create 2D 64-bit (8 bytes) double dataset of 20 by 10
        try {
            if ((group_id2 >= 0) && (dataspace_id1 >= 0)) {
                dataset_id =
                    H5Dcreate2(group_id2, arena.allocateFrom("2D 64-bit double 20x10"), H5T_NATIVE_DOUBLE_g(),
                               dataspace_id1, H5P_DEFAULT(), H5P_DEFAULT(), H5P_DEFAULT());
                if (dataset_id >= 0)
                    H5Dclose(dataset_id);
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // create 3D 32-bit (4 bytes) float dataset of 20 by 10 by 5
        try {
            if ((group_id2 >= 0) && (dataspace_id2 >= 0)) {
                dataset_id = H5Dcreate2(group_id2, arena.allocateFrom("3D 32-bit float  20x10x5"),
                                        H5T_NATIVE_FLOAT_g(), dataspace_id2, H5P_DEFAULT(), H5P_DEFAULT(),
                                        H5P_DEFAULT());
                if (dataset_id >= 0)
                    H5Dclose(dataset_id);
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Terminate access to the data space.
        try {
            if (dataspace_id2 >= 0)
                H5Sclose(dataspace_id2);
        }
        catch (Exception e) {
            e.printStackTrace();
        }
        try {
            if (dataspace_id1 >= 0)
                H5Sclose(dataspace_id1);
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
            HDF5DatasetCreate.CreateDataset(arena);
        }
    }
}
