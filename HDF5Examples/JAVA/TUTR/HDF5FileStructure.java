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

import hdf.hdf5lib.H5;
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

    private static void FileStructure(Arena arena)
    {
        long file_id  = H5I_INVALID_HID();
        long group_id = H5I_INVALID_HID();

        // create the file and add groups and dataset into the file
        try {
            createFile(arena);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Open file using the default properties.
        try {
            file_id = H5Fopen(arena.allocateFrom(fname), H5F_ACC_RDWR(), H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Open the group, obtaining a new handle.
        try {
            if (file_id >= 0)
                group_id = H5Gopen2(file_id, arena.allocateFrom("/"), H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        try {
            printGroup(arena, group_id, "/", "");
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Close the group.
        try {
            if (group_id >= 0)
                H5Gclose(group_id);
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

    /**
     * Recursively print a group and its members.
     *
     * @throws Exception
     */
    private static void printGroup(Arena arena, long g_id, String gname, String indent) throws Exception
    {
        if (g_id < 0)
            return;

        MemorySegment ginfo = arena.allocate(org.hdfgroup.javahdf5.H5G_info_t.sizeof());
        H5Gget_info(g_id, ginfo);
        long nlinks = org.hdfgroup.javahdf5.H5G_info_t.nlinks(ginfo);

        String objNames[]       = new String[(int)nlinks];
        int objTypes[]          = new int[(int)nlinks];
        int lnkTypes[]          = new int[(int)nlinks];
        H5O_token_t objTokens[] = new H5O_token_t[(int)nlinks];
        int names_found         = 0;
        try {
            names_found =
                H5.H5Gget_obj_info_all(g_id, null, objNames, objTypes, lnkTypes, objTokens, H5_INDEX_NAME());
        }
        catch (Throwable err) {
            err.printStackTrace();
        }

        indent += "    ";
        for (int i = 0; i < names_found; i++) {
            System.out.println(indent + objNames[i]);
            long group_id = H5I_INVALID_HID();
            if (objTypes[i] == H5O_TYPE_GROUP()) {
                // Open the group, obtaining a new handle.
                try {
                    if (g_id >= 0)
                        group_id = H5Gopen2(g_id, arena.allocateFrom(objNames[i]), H5P_DEFAULT());
                }
                catch (Exception e) {
                    e.printStackTrace();
                }

                if (group_id >= 0)
                    printGroup(arena, group_id, objNames[i], indent);

                // Close the group.
                try {
                    if (group_id >= 0)
                        H5Gclose(group_id);
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
    private static void createFile(Arena arena) throws Exception
    {
        long[] dims2D      = {20, 10};
        long[] dims3D      = {20, 10, 5};
        long file_id       = H5I_INVALID_HID();
        long dataset_id    = H5I_INVALID_HID();
        long dataspace_id1 = H5I_INVALID_HID();
        long dataspace_id2 = H5I_INVALID_HID();
        long group_id1     = H5I_INVALID_HID();
        long group_id2     = H5I_INVALID_HID();

        // Create a new file using default properties.
        try {
            file_id = H5Fcreate(arena.allocateFrom(fname), H5F_ACC_TRUNC(), H5P_DEFAULT(), H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create groups in the file.
        try {
            if (file_id >= 0) {
                group_id1 = H5Gcreate2(file_id, arena.allocateFrom("/integer arrays"), H5P_DEFAULT(),
                                       H5P_DEFAULT(), H5P_DEFAULT());
                group_id1 = H5Gcreate2(file_id, arena.allocateFrom("/float arrays"), H5P_DEFAULT(),
                                       H5P_DEFAULT(), H5P_DEFAULT());
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create the data space for the datasets.
        try {
            dataspace_id1 =
                H5Screate_simple(2, arena.allocateFrom(ValueLayout.JAVA_LONG, dims2D), MemorySegment.NULL);
            dataspace_id2 =
                H5Screate_simple(3, arena.allocateFrom(ValueLayout.JAVA_LONG, dims3D), MemorySegment.NULL);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // create 2D 32-bit (4 bytes) integer dataset of 20 by 10
        try {
            if ((file_id >= 0) && (dataspace_id1 >= 0))
                dataset_id =
                    H5Dcreate2(file_id, arena.allocateFrom("/integer arrays/2D 32-bit integer 20x10"),
                               H5T_STD_I32LE_g(), dataspace_id1, H5P_DEFAULT(), H5P_DEFAULT(), H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Close the dataset.
        try {
            if (dataset_id >= 0)
                H5Dclose(dataset_id);
            dataset_id = H5I_INVALID_HID();
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // create 3D 8-bit (1 byte) unsigned integer dataset of 20 by 10 by 5
        try {
            if ((file_id >= 0) && (dataspace_id2 >= 0))
                dataset_id = H5Dcreate2(
                    file_id, arena.allocateFrom("/integer arrays/3D 8-bit unsigned integer 20x10x5"),
                    H5T_STD_I64LE_g(), dataspace_id2, H5P_DEFAULT(), H5P_DEFAULT(), H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Close the dataset.
        try {
            if (dataset_id >= 0)
                H5Dclose(dataset_id);
            dataset_id = H5I_INVALID_HID();
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // create 2D 64-bit (8 bytes) double dataset of 20 by 10
        try {
            if ((file_id >= 0) && (dataspace_id1 >= 0))
                dataset_id = H5Dcreate2(file_id, arena.allocateFrom("/float arrays/2D 64-bit double 20x10"),
                                        H5T_NATIVE_DOUBLE_g(), dataspace_id1, H5P_DEFAULT(), H5P_DEFAULT(),
                                        H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Close the dataset.
        try {
            if (dataset_id >= 0)
                H5Dclose(dataset_id);
            dataset_id = H5I_INVALID_HID();
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // create 3D 32-bit (4 bytes) float dataset of 20 by 10 by 5
        try {
            if ((file_id >= 0) && (dataspace_id2 >= 0))
                dataset_id = H5Dcreate2(file_id, arena.allocateFrom("/float arrays/3D 32-bit float  20x10x5"),
                                        H5T_NATIVE_FLOAT_g(), dataspace_id2, H5P_DEFAULT(), H5P_DEFAULT(),
                                        H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Close the dataset.
        try {
            if (dataset_id >= 0)
                H5Dclose(dataset_id);
            dataset_id = H5I_INVALID_HID();
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Close the data space.
        try {
            if (dataspace_id1 >= 0)
                H5Sclose(dataspace_id1);
            dataspace_id1 = H5I_INVALID_HID();
            if (dataspace_id2 >= 0)
                H5Sclose(dataspace_id2);
            dataspace_id2 = H5I_INVALID_HID();
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Close the groups.
        try {
            if (group_id1 >= 0)
                H5Gclose(group_id1);
            if (group_id2 >= 0)
                H5Gclose(group_id2);
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
            HDF5FileStructure.FileStructure(arena);
        }
    }
}
