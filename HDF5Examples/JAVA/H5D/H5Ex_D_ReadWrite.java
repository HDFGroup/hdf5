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
  dataset using the FFM (Foreign Function & Memory) API.
  The program first writes integers to a dataset with
  dataspace dimensions of DIM_XxDIM_Y, then closes the
  file.  Next, it reopens the file, reads back the data,
  and outputs it to the screen.

  This is a pure FFM example showing:
  - Arena-based memory management
  - MemorySegment for strings and arrays
  - Direct FFM API calls (no H5 wrapper)

 ************************************************************/

import static org.hdfgroup.javahdf5.hdf5_h.*;

import java.lang.foreign.Arena;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.ValueLayout;

public class H5Ex_D_ReadWrite {
    private static String FILENAME    = "H5Ex_D_ReadWrite.h5";
    private static String DATASETNAME = "DS1";
    private static final int DIM_X    = 4;
    private static final int DIM_Y    = 7;
    private static final int RANK     = 2;

    private static void WriteDataset(Arena arena)
    {
        long file_id      = H5I_INVALID_HID();
        long filespace_id = H5I_INVALID_HID();
        long dataset_id   = H5I_INVALID_HID();
        long[] dims       = {DIM_X, DIM_Y};
        int[][] dset_data = new int[DIM_X][DIM_Y];

        // Initialize data.
        for (int indx = 0; indx < DIM_X; indx++)
            for (int jndx = 0; jndx < DIM_Y; jndx++)
                dset_data[indx][jndx] = indx * jndx - jndx;

        // Create a new file using default properties.
        try {
            MemorySegment filename = arena.allocateFrom(FILENAME);
            file_id                = H5Fcreate(filename, H5F_ACC_TRUNC(), H5P_DEFAULT(), H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create dataspace. Setting maximum size to NULL sets the maximum
        // size to be the current size.
        try {
            MemorySegment dimsSeg = arena.allocateFrom(ValueLayout.JAVA_LONG, dims);
            filespace_id          = H5Screate_simple(RANK, dimsSeg, MemorySegment.NULL);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create the dataset. We will use all default properties for this example.
        try {
            if ((file_id >= 0) && (filespace_id >= 0)) {
                MemorySegment datasetname = arena.allocateFrom(DATASETNAME);
                dataset_id = H5Dcreate2(file_id, datasetname, H5T_STD_I32LE_g(), filespace_id, H5P_DEFAULT(),
                                        H5P_DEFAULT(), H5P_DEFAULT());
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Write the data to the dataset.
        // FFM requires explicit conversion of 2D array to MemorySegment.
        try {
            if (dataset_id >= 0) {
                // Flatten 2D array to 1D for MemorySegment
                int[] flatData = new int[DIM_X * DIM_Y];
                for (int i = 0; i < DIM_X; i++) {
                    for (int j = 0; j < DIM_Y; j++) {
                        flatData[i * DIM_Y + j] = dset_data[i][j];
                    }
                }

                // Copy flattened data to MemorySegment
                MemorySegment dataSeg = arena.allocate(ValueLayout.JAVA_INT, flatData.length);
                for (int i = 0; i < flatData.length; i++) {
                    dataSeg.setAtIndex(ValueLayout.JAVA_INT, i, flatData[i]);
                }

                H5Dwrite(dataset_id, H5T_NATIVE_INT_g(), H5S_ALL(), H5S_ALL(), H5P_DEFAULT(), dataSeg);
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

    private static void ReadDataset(Arena arena)
    {
        long file_id      = H5I_INVALID_HID();
        long dataset_id   = H5I_INVALID_HID();
        int[][] dset_data = new int[DIM_X][DIM_Y];

        // Open file using the default properties.
        try {
            MemorySegment filename = arena.allocateFrom(FILENAME);
            file_id                = H5Fopen(filename, H5F_ACC_RDWR(), H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Open dataset using the default properties.
        try {
            if (file_id >= 0) {
                MemorySegment datasetname = arena.allocateFrom(DATASETNAME);
                dataset_id                = H5Dopen2(file_id, datasetname, H5P_DEFAULT());
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Read the data using the default properties.
        // FFM requires explicit conversion from MemorySegment to 2D array.
        try {
            if (dataset_id >= 0) {
                // Allocate MemorySegment for reading
                MemorySegment dataSeg = arena.allocate(ValueLayout.JAVA_INT, DIM_X * DIM_Y);

                H5Dread(dataset_id, H5T_NATIVE_INT_g(), H5S_ALL(), H5S_ALL(), H5P_DEFAULT(), dataSeg);

                // Copy from MemorySegment and unflatten to 2D array
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
        System.out.println(DATASETNAME + ":");
        for (int indx = 0; indx < DIM_X; indx++) {
            System.out.print(" [ ");
            for (int jndx = 0; jndx < DIM_Y; jndx++)
                System.out.print(dset_data[indx][jndx] + " ");
            System.out.println("]");
        }
        System.out.println();

        // Close the dataset.
        try {
            if (dataset_id >= 0)
                H5Dclose(dataset_id);
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
        // Arena manages all native memory allocations
        // All allocations are automatically freed when arena closes
        try (Arena arena = Arena.ofConfined()) {
            H5Ex_D_ReadWrite.WriteDataset(arena);
            H5Ex_D_ReadWrite.ReadDataset(arena);
        }
    }
}
