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
  This example shows how to read and write bitfield
  datatypes to a dataset.  The program first writes bit
  fields to a dataset with a dataspace of DIM0xDIM1, then
  closes the file.  Next, it reopens the file, reads back
  the data, and outputs it to the screen.
 ************************************************************/

import static org.hdfgroup.javahdf5.hdf5_h.*;

import java.lang.foreign.Arena;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.ValueLayout;

public class H5Ex_T_Bit {
    private static String FILENAME    = "H5Ex_T_Bit.h5";
    private static String DATASETNAME = "DS1";
    private static final int DIM0     = 4;
    private static final int DIM1     = 7;
    private static final int RANK     = 2;

    private static void CreateDataset(Arena arena)
    {
        long file_id      = H5I_INVALID_HID();
        long dataspace_id = H5I_INVALID_HID();
        long dataset_id   = H5I_INVALID_HID();
        long[] dims       = {DIM0, DIM1};
        int[][] dset_data = new int[DIM0][DIM1];

        // Initialize data.
        for (int indx = 0; indx < DIM0; indx++)
            for (int jndx = 0; jndx < DIM1; jndx++) {
                dset_data[indx][jndx] = 0;
                dset_data[indx][jndx] |= (indx * jndx - jndx) & 0x03; /* Field "A" */
                dset_data[indx][jndx] |= (indx & 0x03) << 2;          /* Field "B" */
                dset_data[indx][jndx] |= (jndx & 0x03) << 4;          /* Field "C" */
                dset_data[indx][jndx] |= ((indx + jndx) & 0x03) << 6; /* Field "D" */
            }

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
            dataspace_id =
                H5Screate_simple(RANK, arena.allocateFrom(ValueLayout.JAVA_LONG, dims), MemorySegment.NULL);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create the dataset.
        try {
            if ((file_id >= 0) && (dataspace_id >= 0))
                dataset_id = H5Dcreate2(file_id, arena.allocateFrom(DATASETNAME), H5T_STD_B8BE_g(),
                                        dataspace_id, H5P_DEFAULT(), H5P_DEFAULT(), H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Write the bitfield data to the dataset.
        try {
            if (dataset_id >= 0) {
                // Flatten the 2D array to 1D for MemorySegment
                int[] flatData = new int[DIM0 * DIM1];
                for (int i = 0; i < DIM0; i++) {
                    for (int j = 0; j < DIM1; j++) {
                        flatData[i * DIM1 + j] = dset_data[i][j];
                    }
                }
                MemorySegment dataSeg = arena.allocateFrom(ValueLayout.JAVA_INT, flatData);
                H5Dwrite(dataset_id, H5T_NATIVE_B8_g(), H5S_ALL(), H5S_ALL(), H5P_DEFAULT(), dataSeg);
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

        // Terminate access to the data space.
        try {
            if (dataspace_id >= 0)
                H5Sclose(dataspace_id);
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
        long dataspace_id = H5I_INVALID_HID();
        long dataset_id   = H5I_INVALID_HID();
        long[] dims       = {DIM0, DIM1};
        int[][] dset_data;

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

        // Get dataspace and allocate memory for read buffer.
        try {
            if (dataset_id >= 0)
                dataspace_id = H5Dget_space(dataset_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        try {
            if (dataspace_id >= 0) {
                MemorySegment dimsSeg = arena.allocateFrom(ValueLayout.JAVA_LONG, dims);
                H5Sget_simple_extent_dims(dataspace_id, dimsSeg, MemorySegment.NULL);
                // Read back the dimensions
                for (int i = 0; i < dims.length; i++) {
                    dims[i] = dimsSeg.getAtIndex(ValueLayout.JAVA_LONG, i);
                }
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Allocate array of pointers to two-dimensional arrays (the
        // elements of the dataset.
        dset_data = new int[(int)dims[0]][(int)(dims[1])];

        // Read data.
        try {
            if (dataset_id >= 0) {
                int totalSize         = (int)(dims[0] * dims[1]);
                MemorySegment dataSeg = arena.allocate(ValueLayout.JAVA_INT, totalSize);
                H5Dread(dataset_id, H5T_NATIVE_B8_g(), H5S_ALL(), H5S_ALL(), H5P_DEFAULT(), dataSeg);
                // Unflatten the 1D MemorySegment to 2D array
                for (int i = 0; i < dims[0]; i++) {
                    for (int j = 0; j < dims[1]; j++) {
                        dset_data[i][j] = dataSeg.getAtIndex(ValueLayout.JAVA_INT, i * (int)dims[1] + j);
                    }
                }
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Output the data to the screen.
        System.out.println(DATASETNAME + ":");
        for (int indx = 0; indx < dims[0]; indx++) {
            System.out.print(" [");
            for (int jndx = 0; jndx < dims[1]; jndx++) {
                System.out.print("{" + (dset_data[indx][jndx] & 0x03) + ", ");
                System.out.print(((dset_data[indx][jndx] >> 2) & 0x03) + ", ");
                System.out.print(((dset_data[indx][jndx] >> 4) & 0x03) + ", ");
                System.out.print(((dset_data[indx][jndx] >> 6) & 0x03) + "}");
            }
            System.out.println("]");
        }
        System.out.println();

        // End access to the dataset and release resources used by it.
        try {
            if (dataset_id >= 0)
                H5Dclose(dataset_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Terminate access to the data space.
        try {
            if (dataspace_id >= 0)
                H5Sclose(dataspace_id);
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
            H5Ex_T_Bit.CreateDataset(arena);
            H5Ex_T_Bit.ReadDataset(arena);
        }
    }
}
