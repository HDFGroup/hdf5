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
    Create two datasets within groups.
 ************************************************************/

import static org.hdfgroup.javahdf5.hdf5_h.*;

import java.lang.foreign.Arena;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.ValueLayout;

public class HDF5GroupDatasetCreate {
    private static String FILENAME     = "HDF5GroupDatasetCreate.h5";
    private static String GROUPNAME    = "MyGroup";
    private static String GROUPNAME_A  = "GroupA";
    private static String DATASETNAME1 = "dset1";
    private static String DATASETNAME2 = "dset2";
    private static final int DIM1_X    = 3;
    private static final int DIM1_Y    = 3;
    private static final int DIM2_X    = 2;
    private static final int DIM2_Y    = 10;

    private static void h5_crtgrpd(Arena arena)
    {
        long file_id       = H5I_INVALID_HID();
        long dataspace_id  = H5I_INVALID_HID();
        long dataset_id    = H5I_INVALID_HID();
        long group_id      = H5I_INVALID_HID();
        long group1_id     = H5I_INVALID_HID();
        long group2_id     = H5I_INVALID_HID();
        int[][] dset1_data = new int[DIM1_X][DIM1_Y];
        int[][] dset2_data = new int[DIM2_X][DIM2_Y];
        long[] dims1       = {DIM1_X, DIM1_Y};
        long[] dims2       = {DIM2_X, DIM2_Y};

        // Initialize the first dataset.
        for (int indx = 0; indx < DIM1_X; indx++)
            for (int jndx = 0; jndx < DIM1_Y; jndx++)
                dset1_data[indx][jndx] = jndx + 1;

        // Initialize the second dataset.
        for (int indx = 0; indx < DIM2_X; indx++)
            for (int jndx = 0; jndx < DIM2_Y; jndx++)
                dset2_data[indx][jndx] = jndx + 1;

        // Create a file.
        try {
            file_id = H5Fcreate(arena.allocateFrom(FILENAME), H5F_ACC_TRUNC(), H5P_DEFAULT(), H5P_DEFAULT());
            // Create a group named "/MyGroup" in the file.
            if (file_id >= 0) {
                group1_id = H5Gcreate2(file_id, arena.allocateFrom("/" + GROUPNAME), H5P_DEFAULT(),
                                       H5P_DEFAULT(), H5P_DEFAULT());
                // Create group "Group_A" in group "MyGroup" using absolute name.
                if (group1_id >= 0) {
                    group2_id = H5Gcreate2(file_id, arena.allocateFrom("/" + GROUPNAME + "/" + GROUPNAME_A),
                                           H5P_DEFAULT(), H5P_DEFAULT(), H5P_DEFAULT());
                    if (group2_id >= 0)
                        H5Gclose(group2_id);
                }
                if (group1_id >= 0)
                    H5Gclose(group1_id);
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create the data space for the first dataset.
        try {
            dataspace_id =
                H5Screate_simple(2, arena.allocateFrom(ValueLayout.JAVA_LONG, dims1), MemorySegment.NULL);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create the dataset in group "MyGroup".
        try {
            if ((file_id >= 0) && (dataspace_id >= 0))
                dataset_id =
                    H5Dcreate2(file_id, arena.allocateFrom("/" + GROUPNAME + "/" + DATASETNAME1),
                               H5T_STD_I32BE_g(), dataspace_id, H5P_DEFAULT(), H5P_DEFAULT(), H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Write the first dataset.
        try {
            if (dataset_id >= 0) {
                // Flatten 2D array to 1D for MemorySegment
                int[] flatData1 = new int[DIM1_X * DIM1_Y];
                for (int i = 0; i < DIM1_X; i++)
                    for (int j = 0; j < DIM1_Y; j++)
                        flatData1[i * DIM1_Y + j] = dset1_data[i][j];
                MemorySegment dataSeg1 = arena.allocateFrom(ValueLayout.JAVA_INT, flatData1);
                H5Dwrite(dataset_id, H5T_NATIVE_INT_g(), H5S_ALL(), H5S_ALL(), H5P_DEFAULT(), dataSeg1);
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Close the data space for the first dataset.
        try {
            if (dataspace_id >= 0)
                H5Sclose(dataspace_id);
            dataspace_id = H5I_INVALID_HID();
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Close the first dataset.
        try {
            if (dataset_id >= 0)
                H5Dclose(dataset_id);
            dataset_id = H5I_INVALID_HID();
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Open an existing group of the specified file.
        try {
            if (file_id >= 0)
                group_id =
                    H5Gopen2(file_id, arena.allocateFrom("/" + GROUPNAME + "/" + GROUPNAME_A), H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create the data space for the second dataset.
        try {
            dataspace_id =
                H5Screate_simple(2, arena.allocateFrom(ValueLayout.JAVA_LONG, dims2), MemorySegment.NULL);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create the second dataset in group "Group_A".
        try {
            if ((group_id >= 0) && (dataspace_id >= 0))
                dataset_id = H5Dcreate2(group_id, arena.allocateFrom(DATASETNAME2), H5T_STD_I32BE_g(),
                                        dataspace_id, H5P_DEFAULT(), H5P_DEFAULT(), H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Write the second dataset.
        try {
            if (dataset_id >= 0) {
                // Flatten 2D array to 1D for MemorySegment
                int[] flatData2 = new int[DIM2_X * DIM2_Y];
                for (int i = 0; i < DIM2_X; i++)
                    for (int j = 0; j < DIM2_Y; j++)
                        flatData2[i * DIM2_Y + j] = dset2_data[i][j];
                MemorySegment dataSeg2 = arena.allocateFrom(ValueLayout.JAVA_INT, flatData2);
                H5Dwrite(dataset_id, H5T_NATIVE_INT_g(), H5S_ALL(), H5S_ALL(), H5P_DEFAULT(), dataSeg2);
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Close the data space for the second dataset.
        try {
            if (dataspace_id >= 0)
                H5Sclose(dataspace_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Close the second dataset.
        try {
            if (dataset_id >= 0)
                H5Dclose(dataset_id);
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

    public static void main(String[] args)
    {
        try (Arena arena = Arena.ofConfined()) {
            HDF5GroupDatasetCreate.h5_crtgrpd(arena);
        }
    }
}
