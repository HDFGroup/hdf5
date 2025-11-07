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
  This example shows how to read and write integer datatypes
  to a dataset.  The program first writes integers to a
  dataset with a dataspace of DIM0xDIM1, then closes the
  file.  Next, it reopens the file, reads back the data, and
  outputs it to the screen.
 ************************************************************/

import static org.hdfgroup.javahdf5.hdf5_h.*;

import java.lang.foreign.Arena;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.ValueLayout;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.Locale;

public class H5Ex_T_Float {
    private static String FILENAME    = "H5Ex_T_Float.h5";
    private static String DATASETNAME = "DS1";
    private static final int DIM0     = 4;
    private static final int DIM1     = 7;
    private static final int RANK     = 2;

    private static void CreateDataset(Arena arena)
    {
        long file_id         = H5I_INVALID_HID();
        long dataspace_id    = H5I_INVALID_HID();
        long dataset_id      = H5I_INVALID_HID();
        long[] dims          = {DIM0, DIM1};
        double[][] dset_data = new double[DIM0][DIM1];

        // Initialize data.
        for (int indx = 0; indx < DIM0; indx++)
            for (int jndx = 0; jndx < DIM1; jndx++) {
                dset_data[indx][jndx] = indx / (jndx + 0.5) + jndx;
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

        // Create the dataset and write the floating point data to it. In
        // this example we will save the data as 64 bit little endian IEEE
        // floating point numbers, regardless of the native type. The HDF5
        // library automatically converts between different floating point
        // types.
        try {
            if ((file_id >= 0) && (dataspace_id >= 0))
                dataset_id = H5Dcreate2(file_id, arena.allocateFrom(DATASETNAME), H5T_IEEE_F64LE_g(),
                                        dataspace_id, H5P_DEFAULT(), H5P_DEFAULT(), H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Write the data to the dataset.
        try {
            if (dataset_id >= 0) {
                // Flatten the 2D array to 1D for MemorySegment
                double[] flatData = new double[4 * 7];
                for (int i = 0; i < 4; i++) {
                    for (int j = 0; j < 7; j++) {
                        flatData[i * 7 + j] = dset_data[i][j];
                    }
                }
                MemorySegment dataSeg = arena.allocateFrom(ValueLayout.JAVA_DOUBLE, flatData);
                H5Dwrite(dataset_id, H5T_NATIVE_DOUBLE_g(), H5S_ALL(), H5S_ALL(), H5P_DEFAULT(), dataSeg);
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
        double[][] dset_data;

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
        dset_data = new double[(int)dims[0]][(int)(dims[1])];

        // Read data.
        try {
            if (dataset_id >= 0) {
                int totalSize         = 4 * 7;
                MemorySegment dataSeg = arena.allocate(ValueLayout.JAVA_DOUBLE, totalSize);
                H5Dread(dataset_id, H5T_NATIVE_DOUBLE_g(), H5S_ALL(), H5S_ALL(), H5P_DEFAULT(), dataSeg);
                // Unflatten the 1D MemorySegment to 2D array
                for (int i = 0; i < 4; i++) {
                    for (int j = 0; j < 7; j++) {
                        dset_data[i][j] = dataSeg.getAtIndex(ValueLayout.JAVA_DOUBLE, i * 7 + j);
                    }
                }
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Output the data to the screen.
        DecimalFormat df = new DecimalFormat("#,##0.0000", new DecimalFormatSymbols(Locale.US));
        System.out.println(DATASETNAME + ":");
        for (int indx = 0; indx < dims[0]; indx++) {
            System.out.print(" [");
            for (int jndx = 0; jndx < dims[1]; jndx++) {
                System.out.print(" " + df.format(dset_data[indx][jndx]));
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
            H5Ex_T_Float.CreateDataset(arena);
            H5Ex_T_Float.ReadDataset(arena);
        }
    }
}
