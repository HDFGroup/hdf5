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
      This example shows how to read and write data to a dataset
      using a data transform expression.  The program first
      writes integers to a dataset using the transform
      expression TRANSFORM, then closes the file.  Next, it
      reopens the file, reads back the data without a transform,
      and outputs the data to the screen.  Finally it reads the
      data using the transform expression RTRANSFORM and outputs
      the results to the screen.
 ************************************************************/

import static org.hdfgroup.javahdf5.hdf5_h.*;

import java.lang.foreign.Arena;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.ValueLayout;

public class H5Ex_D_Transform {

    private static String FILENAME   = "H5Ex_D_Transform.h5";
    private static String DATASET    = "DS1";
    private static final int DIM_X   = 4;
    private static final int DIM_Y   = 7;
    private static String TRANSFORM  = "x+1";
    private static String RTRANSFORM = "x-1";

    private static void writeData(Arena arena)
    {
        long file_id      = H5I_INVALID_HID();
        long filespace_id = H5I_INVALID_HID();
        long dataset_id   = H5I_INVALID_HID();
        long dxpl_id      = H5I_INVALID_HID();

        long[] dims       = {DIM_X, DIM_Y};
        int[][] dset_data = new int[DIM_X][DIM_Y];

        // Initialize data.
        for (int i = 0; i < DIM_X; i++)
            for (int j = 0; j < DIM_Y; j++)
                dset_data[i][j] = i * j - j;

        // Output the data to the screen.
        System.out.println("Original Data:");
        for (int i = 0; i < DIM_X; i++) {
            System.out.print(" [");
            for (int j = 0; j < DIM_Y; j++)
                System.out.print(" " + dset_data[i][j] + " ");
            System.out.println("]");
        }

        // Create a new file using the default properties.
        try {
            file_id = H5Fcreate(arena.allocateFrom(FILENAME), H5F_ACC_TRUNC(), H5P_DEFAULT(), H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create dataspace. Setting maximum size to NULL sets the maximum
        // size to be the current size.
        try {
            filespace_id =
                H5Screate_simple(2, arena.allocateFrom(ValueLayout.JAVA_LONG, dims), MemorySegment.NULL);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create the dataset transfer property list and define the transform expression.
        try {
            dxpl_id = H5Pcreate(H5P_CLS_DATASET_XFER_ID_g());
            if (dxpl_id >= 0)
                H5Pset_data_transform(dxpl_id, arena.allocateFrom(TRANSFORM));
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create the dataset using the default properties. Unfortunately we must save as
        // a native type or the transform operation will fail.
        try {
            if ((file_id >= 0) && (filespace_id >= 0))
                dataset_id = H5Dcreate2(file_id, arena.allocateFrom(DATASET), H5T_NATIVE_INT_g(),
                                        filespace_id, H5P_DEFAULT(), H5P_DEFAULT(), H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Write the data to the dataset using the dataset transfer property list.
        try {
            if ((dataset_id >= 0) && (dxpl_id >= 0)) {
                // Flatten 2D array for FFM
                int[] flatData = new int[DIM_X * DIM_Y];
                for (int i = 0; i < DIM_X; i++) {
                    for (int j = 0; j < DIM_Y; j++) {
                        flatData[i * DIM_Y + j] = dset_data[i][j];
                    }
                }
                MemorySegment dataSeg = arena.allocate(ValueLayout.JAVA_INT, flatData.length);
                for (int i = 0; i < flatData.length; i++) {
                    dataSeg.setAtIndex(ValueLayout.JAVA_INT, i, flatData[i]);
                }
                H5Dwrite(dataset_id, H5T_NATIVE_INT_g(), H5S_ALL(), H5S_ALL(), dxpl_id, dataSeg);
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // End access to the dataset and release resources used by it.
        try {
            if (dxpl_id >= 0)
                H5Pclose(dxpl_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        try {
            if (dataset_id >= 0)
                H5Dclose(dataset_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        try {
            if (filespace_id >= 0)
                H5Sclose(filespace_id);
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

    private static void readData(Arena arena)
    {

        long file_id      = H5I_INVALID_HID();
        long dataset_id   = H5I_INVALID_HID();
        long dxpl_id      = H5I_INVALID_HID();
        int[][] dset_data = new int[DIM_X][DIM_Y];

        // Open an existing file using the default properties.
        try {
            file_id = H5Fopen(arena.allocateFrom(FILENAME), H5F_ACC_RDONLY(), H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Open an existing dataset using the default properties.
        try {
            if (file_id >= 0)
                dataset_id = H5Dopen2(file_id, arena.allocateFrom(DATASET), H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Read the data using the default properties.
        try {
            if (dataset_id >= 0) {
                MemorySegment dataSeg = arena.allocate(ValueLayout.JAVA_INT, DIM_X * DIM_Y);
                H5Dread(dataset_id, H5T_NATIVE_INT_g(), H5S_ALL(), H5S_ALL(), H5P_DEFAULT(), dataSeg);
                // Unflatten to 2D array
                for (int i = 0; i < DIM_X; i++) {
                    for (int j = 0; j < DIM_Y; j++) {
                        dset_data[i][j] = dataSeg.getAtIndex(ValueLayout.JAVA_INT, i * DIM_Y + j);
                    }
                }
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Output the data to the screen.
        System.out.println("Data as written with transform '" + TRANSFORM + "'");
        for (int i = 0; i < DIM_X; i++) {
            System.out.print(" [");
            for (int j = 0; j < DIM_Y; j++)
                System.out.print(" " + dset_data[i][j] + " ");
            System.out.println("]");
        }

        // Create the dataset transfer property list and define the transform expression.
        try {
            dxpl_id = H5Pcreate(H5P_CLS_DATASET_XFER_ID_g());
            if (dxpl_id >= 0)
                H5Pset_data_transform(dxpl_id, arena.allocateFrom(RTRANSFORM));
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Read the data using the dataset transfer property list.
        try {
            if ((dataset_id >= 0) && (dxpl_id >= 0)) {
                MemorySegment dataSeg = arena.allocate(ValueLayout.JAVA_INT, DIM_X * DIM_Y);
                H5Dread(dataset_id, H5T_NATIVE_INT_g(), H5S_ALL(), H5S_ALL(), dxpl_id, dataSeg);
                // Unflatten to 2D array
                for (int i = 0; i < DIM_X; i++) {
                    for (int j = 0; j < DIM_Y; j++) {
                        dset_data[i][j] = dataSeg.getAtIndex(ValueLayout.JAVA_INT, i * DIM_Y + j);
                    }
                }
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Output the data to the screen.

        System.out.println("Data as written with transform  '" + TRANSFORM + "' and read with transform  '" +
                           RTRANSFORM + "'");
        for (int i = 0; i < DIM_X; i++) {
            System.out.print(" [");
            for (int j = 0; j < DIM_Y; j++)
                System.out.print(" " + dset_data[i][j] + " ");
            System.out.println("]");
        }

        // Close and release resources.
        try {
            if (dxpl_id >= 0)
                H5Pclose(dxpl_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }
        try {
            if (dataset_id >= 0)
                H5Dclose(dataset_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }
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
            H5Ex_D_Transform.writeData(arena);
            H5Ex_D_Transform.readData(arena);
        }
    }
}
