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
  This example shows how to set the fill value for a
  dataset.  The program first sets the fill value to
  FILLVAL, creates a dataset with dimensions of DIM_XxDIM_Y,
  reads from the uninitialized dataset, and outputs the
  contents to the screen.  Next, it writes integers to the
  dataset, reads the data back, and outputs it to the
  screen.  Finally it extends the dataset, reads from it,
  and outputs the result to the screen.
 ************************************************************/

import static org.hdfgroup.javahdf5.hdf5_h.*;

import java.lang.foreign.Arena;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.ValueLayout;

public class H5Ex_D_FillValue {
    private static String FILENAME    = "H5Ex_D_FillValue.h5";
    private static String DATASETNAME = "ExtendibleArray";
    private static final int DIM_X    = 4;
    private static final int DIM_Y    = 7;
    private static final int EDIM_X   = 6;
    private static final int EDIM_Y   = 10;
    private static final int CHUNK_X  = 4;
    private static final int CHUNK_Y  = 4;
    private static final int RANK     = 2;
    private static final int NDIMS    = 2;
    private static final int FILLVAL  = 99;

    private static void fillValue(Arena arena)
    {
        long file_id             = H5I_INVALID_HID();
        long dcpl_id             = H5I_INVALID_HID();
        long dataspace_id        = H5I_INVALID_HID();
        long dataset_id          = H5I_INVALID_HID();
        long[] dims              = {DIM_X, DIM_Y};
        long[] extdims           = {EDIM_X, EDIM_Y};
        long[] chunk_dims        = {CHUNK_X, CHUNK_Y};
        long[] maxdims           = {H5S_UNLIMITED(), H5S_UNLIMITED()};
        int[][] write_dset_data  = new int[DIM_X][DIM_Y];
        int[][] read_dset_data   = new int[DIM_X][DIM_Y];
        int[][] extend_dset_data = new int[EDIM_X][EDIM_Y];

        // Initialize the dataset.
        for (int indx = 0; indx < DIM_X; indx++)
            for (int jndx = 0; jndx < DIM_Y; jndx++)
                write_dset_data[indx][jndx] = indx * jndx - jndx;

        // Create a new file using default properties.
        try {
            MemorySegment filename = arena.allocateFrom(FILENAME);
            file_id                = H5Fcreate(filename, H5F_ACC_TRUNC(), H5P_DEFAULT(), H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create dataspace with unlimited dimensions.
        try {
            MemorySegment dimsSeg    = arena.allocateFrom(ValueLayout.JAVA_LONG, dims);
            MemorySegment maxdimsSeg = arena.allocateFrom(ValueLayout.JAVA_LONG, maxdims);
            dataspace_id             = H5Screate_simple(RANK, dimsSeg, maxdimsSeg);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create the dataset creation property list.
        try {
            dcpl_id = H5Pcreate(H5P_CLS_DATASET_CREATE_ID_g());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Set the chunk size.
        try {
            if (dcpl_id >= 0) {
                MemorySegment chunkDimsSeg = arena.allocateFrom(ValueLayout.JAVA_LONG, chunk_dims);
                H5Pset_chunk(dcpl_id, NDIMS, chunkDimsSeg);
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Set the fill value for the dataset
        try {
            if (dcpl_id >= 0) {
                int[] fill_value           = {FILLVAL};
                MemorySegment fillValueSeg = arena.allocateFrom(ValueLayout.JAVA_INT, fill_value);
                H5Pset_fill_value(dcpl_id, H5T_NATIVE_INT_g(), fillValueSeg);
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Set the allocation time to "early". This way we can be sure
        // that reading from the dataset immediately after creation will
        // return the fill value.
        try {
            if (dcpl_id >= 0)
                H5Pset_alloc_time(dcpl_id, H5D_ALLOC_TIME_EARLY());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create the dataset using the dataset creation property list.
        try {
            if ((file_id >= 0) && (dataspace_id >= 0) && (dcpl_id >= 0)) {
                MemorySegment datasetname = arena.allocateFrom(DATASETNAME);
                dataset_id = H5Dcreate2(file_id, datasetname, H5T_STD_I32LE_g(), dataspace_id, H5P_DEFAULT(),
                                        dcpl_id, H5P_DEFAULT());
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Read values from the dataset, which has not been written to yet.
        try {
            if (dataset_id >= 0) {
                MemorySegment dataSeg = arena.allocate(ValueLayout.JAVA_INT, DIM_X * DIM_Y);
                H5Dread(dataset_id, H5T_NATIVE_INT_g(), H5S_ALL(), H5S_ALL(), H5P_DEFAULT(), dataSeg);
                // Unflatten to 2D array
                for (int i = 0; i < DIM_X; i++) {
                    for (int j = 0; j < DIM_Y; j++) {
                        read_dset_data[i][j] = dataSeg.getAtIndex(ValueLayout.JAVA_INT, i * DIM_Y + j);
                    }
                }
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Output the data to the screen.
        System.out.println("Dataset before being written to:");
        for (int indx = 0; indx < DIM_X; indx++) {
            System.out.print(" [ ");
            for (int jndx = 0; jndx < DIM_Y; jndx++)
                System.out.print(read_dset_data[indx][jndx] + " ");
            System.out.println("]");
        }
        System.out.println();

        // Write the data to the dataset.
        try {
            if (dataset_id >= 0) {
                // Flatten 2D array for FFM
                int[] flatData = new int[DIM_X * DIM_Y];
                for (int i = 0; i < DIM_X; i++) {
                    for (int j = 0; j < DIM_Y; j++) {
                        flatData[i * DIM_Y + j] = write_dset_data[i][j];
                    }
                }
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

        // Read the data back.
        try {
            if (dataset_id >= 0) {
                MemorySegment dataSeg = arena.allocate(ValueLayout.JAVA_INT, DIM_X * DIM_Y);
                H5Dread(dataset_id, H5T_NATIVE_INT_g(), H5S_ALL(), H5S_ALL(), H5P_DEFAULT(), dataSeg);
                // Unflatten to 2D array
                for (int i = 0; i < DIM_X; i++) {
                    for (int j = 0; j < DIM_Y; j++) {
                        read_dset_data[i][j] = dataSeg.getAtIndex(ValueLayout.JAVA_INT, i * DIM_Y + j);
                    }
                }
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Output the data to the screen.
        System.out.println("Dataset after being written to:");
        for (int indx = 0; indx < DIM_X; indx++) {
            System.out.print(" [ ");
            for (int jndx = 0; jndx < DIM_Y; jndx++)
                System.out.print(read_dset_data[indx][jndx] + " ");
            System.out.println("]");
        }
        System.out.println();

        // Extend the dataset.
        try {
            if (dataset_id >= 0) {
                MemorySegment extdimsSeg = arena.allocateFrom(ValueLayout.JAVA_LONG, extdims);
                H5Dset_extent(dataset_id, extdimsSeg);
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Read from the extended dataset.
        try {
            if (dataset_id >= 0) {
                MemorySegment dataSeg = arena.allocate(ValueLayout.JAVA_INT, EDIM_X * EDIM_Y);
                H5Dread(dataset_id, H5T_NATIVE_INT_g(), H5S_ALL(), H5S_ALL(), H5P_DEFAULT(), dataSeg);
                // Unflatten to 2D array
                for (int i = 0; i < EDIM_X; i++) {
                    for (int j = 0; j < EDIM_Y; j++) {
                        extend_dset_data[i][j] = dataSeg.getAtIndex(ValueLayout.JAVA_INT, i * EDIM_Y + j);
                    }
                }
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Output the data to the screen.
        System.out.println("Dataset after extension:");
        for (int indx = 0; indx < EDIM_X; indx++) {
            System.out.print(" [ ");
            for (int jndx = 0; jndx < EDIM_Y; jndx++)
                System.out.print(extend_dset_data[indx][jndx] + " ");
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

        try {
            if (dataspace_id >= 0)
                H5Sclose(dataspace_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        try {
            if (dcpl_id >= 0)
                H5Pclose(dcpl_id);
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
            H5Ex_D_FillValue.fillValue(arena);
        }
    }
}
