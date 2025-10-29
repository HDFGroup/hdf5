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
  This example shows how to read and write data to a
  dataset by hyberslabs.  The program first writes integers
  in a hyperslab selection to a dataset with dataspace
  dimensions of DIM_XxDIM_Y, then closes the file.  Next, it
  reopens the file, reads back the data, and outputs it to
  the screen.  Finally it reads the data again using a
  different hyperslab selection, and outputs the result to
  the screen.
 ************************************************************/

import static org.hdfgroup.javahdf5.hdf5_h.*;

import java.lang.foreign.Arena;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.ValueLayout;

public class H5Ex_D_Hyperslab {
    private static String FILENAME    = "H5Ex_D_Hyperslab.h5";
    private static String DATASETNAME = "DS1";
    private static final int DIM_X    = 6;
    private static final int DIM_Y    = 8;
    private static final int RANK     = 2;

    private static void writeHyperslab(Arena arena)
    {
        long file_id      = H5I_INVALID_HID();
        long filespace_id = H5I_INVALID_HID();
        long dataset_id   = H5I_INVALID_HID();
        long[] dims       = {DIM_X, DIM_Y};
        int[][] dset_data = new int[DIM_X][DIM_Y];

        // Initialize data to "1", to make it easier to see the selections.
        for (int indx = 0; indx < DIM_X; indx++)
            for (int jndx = 0; jndx < DIM_Y; jndx++)
                dset_data[indx][jndx] = 1;

        // Print the data to the screen.
        System.out.println("Original Data:");
        for (int indx = 0; indx < DIM_X; indx++) {
            System.out.print(" [ ");
            for (int jndx = 0; jndx < DIM_Y; jndx++)
                System.out.print(dset_data[indx][jndx] + " ");
            System.out.println("]");
        }
        System.out.println();

        // Create a new file using default properties.
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
                H5Screate_simple(RANK, arena.allocateFrom(ValueLayout.JAVA_LONG, dims), MemorySegment.NULL);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create the dataset. We will use all default properties for this example.
        try {
            if ((file_id >= 0) && (filespace_id >= 0))
                dataset_id = H5Dcreate2(file_id, arena.allocateFrom(DATASETNAME), H5T_STD_I32LE_g(),
                                        filespace_id, H5P_DEFAULT(), H5P_DEFAULT(), H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Define and select the first part of the hyperslab selection.
        long[] start  = {0, 0};
        long[] stride = {3, 3};
        long[] count  = {2, 3};
        long[] block  = {2, 2};
        try {
            if ((filespace_id >= 0)) {
                MemorySegment startSeg  = arena.allocateFrom(ValueLayout.JAVA_LONG, start);
                MemorySegment strideSeg = arena.allocateFrom(ValueLayout.JAVA_LONG, stride);
                MemorySegment countSeg  = arena.allocateFrom(ValueLayout.JAVA_LONG, count);
                MemorySegment blockSeg  = arena.allocateFrom(ValueLayout.JAVA_LONG, block);
                H5Sselect_hyperslab(filespace_id, H5S_SELECT_SET(), startSeg, strideSeg, countSeg, blockSeg);
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }
        // Define and select the second part of the hyperslab selection,
        // which is subtracted from the first selection by the use of
        // H5S_SELECT_NOTB
        block[0] = 1;
        block[1] = 1;
        try {
            if ((filespace_id >= 0)) {
                MemorySegment startSeg  = arena.allocateFrom(ValueLayout.JAVA_LONG, start);
                MemorySegment strideSeg = arena.allocateFrom(ValueLayout.JAVA_LONG, stride);
                MemorySegment countSeg  = arena.allocateFrom(ValueLayout.JAVA_LONG, count);
                MemorySegment blockSeg  = arena.allocateFrom(ValueLayout.JAVA_LONG, block);
                H5Sselect_hyperslab(filespace_id, H5S_SELECT_NOTB(), startSeg, strideSeg, countSeg, blockSeg);

                // Write the data to the dataset.
                if (dataset_id >= 0) {
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
                    H5Dwrite(dataset_id, H5T_NATIVE_INT_g(), H5S_ALL(), filespace_id, H5P_DEFAULT(), dataSeg);
                }
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // End access to the dataset and release resources used by it.
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

    private static void readHyperslab(Arena arena)
    {
        long file_id      = H5I_INVALID_HID();
        long filespace_id = H5I_INVALID_HID();
        long dataset_id   = H5I_INVALID_HID();
        long dcpl_id      = H5I_INVALID_HID();
        int[][] dset_data = new int[DIM_X][DIM_Y];

        // Open an existing file.
        try {
            file_id = H5Fopen(arena.allocateFrom(FILENAME), H5F_ACC_RDONLY(), H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Open an existing dataset.
        try {
            if (file_id >= 0)
                dataset_id = H5Dopen2(file_id, arena.allocateFrom(DATASETNAME), H5P_DEFAULT());
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
        System.out.println("Data as written to disk by hyberslabs:");
        for (int indx = 0; indx < DIM_X; indx++) {
            System.out.print(" [ ");
            for (int jndx = 0; jndx < DIM_Y; jndx++)
                System.out.print(dset_data[indx][jndx] + " ");
            System.out.println("]");
        }
        System.out.println();

        // Initialize the read array.
        for (int indx = 0; indx < DIM_X; indx++)
            for (int jndx = 0; jndx < DIM_Y; jndx++)
                dset_data[indx][jndx] = 0;

        // Define and select the hyperslab to use for reading.
        try {
            if (dataset_id >= 0) {
                filespace_id = H5Dget_space(dataset_id);

                long[] start  = {0, 1};
                long[] stride = {4, 4};
                long[] count  = {2, 2};
                long[] block  = {2, 3};

                if (filespace_id >= 0) {
                    MemorySegment startSeg  = arena.allocateFrom(ValueLayout.JAVA_LONG, start);
                    MemorySegment strideSeg = arena.allocateFrom(ValueLayout.JAVA_LONG, stride);
                    MemorySegment countSeg  = arena.allocateFrom(ValueLayout.JAVA_LONG, count);
                    MemorySegment blockSeg  = arena.allocateFrom(ValueLayout.JAVA_LONG, block);
                    H5Sselect_hyperslab(filespace_id, H5S_SELECT_SET(), startSeg, strideSeg, countSeg,
                                        blockSeg);

                    // Read the data using the previously defined hyperslab.
                    if ((dataset_id >= 0) && (filespace_id >= 0)) {
                        MemorySegment dataSeg = arena.allocate(ValueLayout.JAVA_INT, DIM_X * DIM_Y);
                        H5Dread(dataset_id, H5T_NATIVE_INT_g(), H5S_ALL(), filespace_id, H5P_DEFAULT(),
                                dataSeg);
                        // Unflatten to 2D array
                        for (int i = 0; i < DIM_X; i++) {
                            for (int j = 0; j < DIM_Y; j++) {
                                dset_data[i][j] = dataSeg.getAtIndex(ValueLayout.JAVA_INT, i * DIM_Y + j);
                            }
                        }
                    }
                }
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Output the data to the screen.
        System.out.println("Data as read from disk by hyberslab:");
        for (int indx = 0; indx < DIM_X; indx++) {
            System.out.print(" [ ");
            for (int jndx = 0; jndx < DIM_Y; jndx++)
                System.out.print(dset_data[indx][jndx] + " ");
            System.out.println("]");
        }
        System.out.println();

        // End access to the dataset and release resources used by it.
        try {
            if (dcpl_id >= 0)
                H5Pclose(dcpl_id);
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

    public static void main(String[] args)
    {
        try (Arena arena = Arena.ofConfined()) {
            H5Ex_D_Hyperslab.writeHyperslab(arena);
            H5Ex_D_Hyperslab.readHyperslab(arena);
        }
    }
}
