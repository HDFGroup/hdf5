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
  This example shows how to set the space allocation time
  for a dataset.  The program first creates two datasets,
  one with the default allocation time (late) and one with
  early allocation time, and displays whether each has been
  allocated and their allocation size.  Next, it writes data
  to the datasets, and again displays whether each has been
  allocated and their allocation size.
 ************************************************************/

import static org.hdfgroup.javahdf5.hdf5_h.*;

import java.lang.foreign.Arena;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.ValueLayout;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;

public class H5Ex_D_Alloc {
    private static String FILENAME     = "H5Ex_D_Alloc.h5";
    private static String DATASETNAME1 = "DS1";
    private static String DATASETNAME2 = "DS2";
    private static final int DIM_X     = 4;
    private static final int DIM_Y     = 7;
    private static final int FILLVAL   = 99;
    private static final int RANK      = 2;

    // Values for the status of space allocation
    enum H5D_space_status {
        H5D_SPACE_STATUS_ERROR(-1),
        H5D_SPACE_STATUS_NOT_ALLOCATED(0),
        H5D_SPACE_STATUS_PART_ALLOCATED(1),
        H5D_SPACE_STATUS_ALLOCATED(2);
        private static final Map<Integer, H5D_space_status> lookup = new HashMap<Integer, H5D_space_status>();

        static
        {
            for (H5D_space_status s : EnumSet.allOf(H5D_space_status.class))
                lookup.put(s.getCode(), s);
        }

        private int code;

        H5D_space_status(int space_status) { this.code = space_status; }

        public int getCode() { return this.code; }

        public static H5D_space_status get(int code) { return lookup.get(code); }
    }

    private static void allocation(Arena arena)
    {
        long file_id      = H5I_INVALID_HID();
        long filespace_id = H5I_INVALID_HID();
        long dataset_id1  = H5I_INVALID_HID();
        long dataset_id2  = H5I_INVALID_HID();
        long dcpl_id      = H5I_INVALID_HID();
        long[] dims       = {DIM_X, DIM_Y};
        int[][] dset_data = new int[DIM_X][DIM_Y];
        int space_status  = 0;
        long storage_size = 0;

        // Initialize the dataset.
        for (int indx = 0; indx < DIM_X; indx++)
            for (int jndx = 0; jndx < DIM_Y; jndx++)
                dset_data[indx][jndx] = FILLVAL;

        // Create a file using default properties.
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

        // Create the dataset creation property list, and set the chunk size.
        try {
            dcpl_id = H5Pcreate(H5P_CLS_DATASET_CREATE_ID_g());
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

        System.out.println("Creating datasets...");
        System.out.println(DATASETNAME1 + " has allocation time H5D_ALLOC_TIME_LATE");
        System.out.println(DATASETNAME2 + " has allocation time H5D_ALLOC_TIME_EARLY");
        System.out.println();

        // Create the dataset using the dataset default creation property list.
        try {
            if ((file_id >= 0) && (filespace_id >= 0)) {
                MemorySegment datasetname1 = arena.allocateFrom(DATASETNAME1);
                dataset_id1 = H5Dcreate2(file_id, datasetname1, H5T_NATIVE_INT_g(), filespace_id,
                                         H5P_DEFAULT(), H5P_DEFAULT(), H5P_DEFAULT());
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create the dataset using the dataset creation property list.
        try {
            if ((file_id >= 0) && (filespace_id >= 0) && (dcpl_id >= 0)) {
                MemorySegment datasetname2 = arena.allocateFrom(DATASETNAME2);
                dataset_id2 = H5Dcreate2(file_id, datasetname2, H5T_NATIVE_INT_g(), filespace_id,
                                         H5P_DEFAULT(), dcpl_id, H5P_DEFAULT());
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Retrieve and print space status and storage size for dset1.
        try {
            if (dataset_id1 >= 0) {
                MemorySegment statusSeg = arena.allocate(ValueLayout.JAVA_INT);
                H5Dget_space_status(dataset_id1, statusSeg);
                space_status = statusSeg.get(ValueLayout.JAVA_INT, 0);
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }
        try {
            if (dataset_id1 >= 0)
                storage_size = H5Dget_storage_size(dataset_id1);
        }
        catch (Exception e) {
            e.printStackTrace();
        }
        String the_space = " ";
        if (H5D_space_status.get(space_status) != H5D_space_status.H5D_SPACE_STATUS_ALLOCATED)
            the_space += "not ";
        System.out.println("Space for " + DATASETNAME1 + " has" + the_space + "been allocated.");
        System.out.println("Storage size for " + DATASETNAME1 + " is: " + storage_size + " bytes.");

        // Retrieve and print space status and storage size for dset2.
        try {
            if (dataset_id2 >= 0) {
                MemorySegment statusSeg = arena.allocate(ValueLayout.JAVA_INT);
                H5Dget_space_status(dataset_id2, statusSeg);
                space_status = statusSeg.get(ValueLayout.JAVA_INT, 0);
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }
        try {
            if (dataset_id2 >= 0)
                storage_size = H5Dget_storage_size(dataset_id2);
        }
        catch (Exception e) {
            e.printStackTrace();
        }
        the_space = " ";
        if (H5D_space_status.get(space_status) != H5D_space_status.H5D_SPACE_STATUS_ALLOCATED)
            the_space += "not ";
        System.out.println("Space for " + DATASETNAME2 + " has" + the_space + "been allocated.");
        System.out.println("Storage size for " + DATASETNAME2 + " is: " + storage_size + " bytes.");
        System.out.println();

        System.out.println("Writing data...");
        System.out.println();

        // Write the data to the datasets.
        try {
            if (dataset_id1 >= 0) {
                // Flatten 2D array to 1D for MemorySegment
                int[] flatData = new int[DIM_X * DIM_Y];
                for (int i = 0; i < DIM_X; i++) {
                    for (int j = 0; j < DIM_Y; j++) {
                        flatData[i * DIM_Y + j] = dset_data[i][j];
                    }
                }
                // Copy to MemorySegment
                MemorySegment dataSeg = arena.allocate(ValueLayout.JAVA_INT, flatData.length);
                for (int i = 0; i < flatData.length; i++) {
                    dataSeg.setAtIndex(ValueLayout.JAVA_INT, i, flatData[i]);
                }
                H5Dwrite(dataset_id1, H5T_NATIVE_INT_g(), H5S_ALL(), H5S_ALL(), H5P_DEFAULT(), dataSeg);
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }
        try {
            if (dataset_id2 >= 0) {
                // Flatten 2D array to 1D for MemorySegment
                int[] flatData = new int[DIM_X * DIM_Y];
                for (int i = 0; i < DIM_X; i++) {
                    for (int j = 0; j < DIM_Y; j++) {
                        flatData[i * DIM_Y + j] = dset_data[i][j];
                    }
                }
                // Copy to MemorySegment
                MemorySegment dataSeg = arena.allocate(ValueLayout.JAVA_INT, flatData.length);
                for (int i = 0; i < flatData.length; i++) {
                    dataSeg.setAtIndex(ValueLayout.JAVA_INT, i, flatData[i]);
                }
                H5Dwrite(dataset_id2, H5T_NATIVE_INT_g(), H5S_ALL(), H5S_ALL(), H5P_DEFAULT(), dataSeg);
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Retrieve and print space status and storage size for dset1.
        try {
            if (dataset_id1 >= 0) {
                MemorySegment statusSeg = arena.allocate(ValueLayout.JAVA_INT);
                H5Dget_space_status(dataset_id1, statusSeg);
                space_status = statusSeg.get(ValueLayout.JAVA_INT, 0);
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }
        try {
            if (dataset_id1 >= 0)
                storage_size = H5Dget_storage_size(dataset_id1);
        }
        catch (Exception e) {
            e.printStackTrace();
        }
        the_space = " ";
        if (H5D_space_status.get(space_status) != H5D_space_status.H5D_SPACE_STATUS_ALLOCATED)
            the_space += "not ";
        System.out.println("Space for " + DATASETNAME1 + " has" + the_space + "been allocated.");
        System.out.println("Storage size for " + DATASETNAME1 + " is: " + storage_size + " bytes.");

        // Retrieve and print space status and storage size for dset2.
        try {
            if (dataset_id2 >= 0) {
                MemorySegment statusSeg = arena.allocate(ValueLayout.JAVA_INT);
                H5Dget_space_status(dataset_id2, statusSeg);
                space_status = statusSeg.get(ValueLayout.JAVA_INT, 0);
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }
        try {
            if (dataset_id2 >= 0)
                storage_size = H5Dget_storage_size(dataset_id2);
        }
        catch (Exception e) {
            e.printStackTrace();
        }
        the_space = " ";
        if (H5D_space_status.get(space_status) != H5D_space_status.H5D_SPACE_STATUS_ALLOCATED)
            the_space += "not ";
        System.out.println("Space for " + DATASETNAME2 + " has" + the_space + "been allocated.");
        System.out.println("Storage size for " + DATASETNAME2 + " is: " + storage_size + " bytes.");
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
            if (dataset_id1 >= 0)
                H5Dclose(dataset_id1);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        try {
            if (dataset_id2 >= 0)
                H5Dclose(dataset_id2);
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
            H5Ex_D_Alloc.allocation(arena);
        }
    }
}
