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
  This example shows how to read and write data to an
  external dataset.  The program first writes integers to an
  external dataset with dataspace dimensions of DIM_XxDIM_Y,
  then closes the file.  Next, it reopens the file, reads
  back the data, and outputs the name of the external data
  file and the data to the screen.
 ************************************************************/

import static org.hdfgroup.javahdf5.hdf5_h.*;

import java.lang.foreign.Arena;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.ValueLayout;

public class H5Ex_D_External {
    private static String FILENAME         = "H5Ex_D_External.h5";
    private static String EXTERNALNAME     = "H5Ex_D_External.data";
    private static String DATASETNAME      = "DS1";
    private static final int DIM_X         = 4;
    private static final int DIM_Y         = 7;
    private static final int RANK          = 2;
    private static final int NAME_BUF_SIZE = 32;

    private static void writeExternal(Arena arena)
    {
        long file_id      = H5I_INVALID_HID();
        long dcpl_id      = H5I_INVALID_HID();
        long filespace_id = H5I_INVALID_HID();
        long dataset_id   = H5I_INVALID_HID();
        long[] dims       = {DIM_X, DIM_Y};
        int[][] dset_data = new int[DIM_X][DIM_Y];

        // Initialize the dataset.
        for (int indx = 0; indx < DIM_X; indx++)
            for (int jndx = 0; jndx < DIM_Y; jndx++)
                dset_data[indx][jndx] = indx * jndx - jndx;

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

        // Create the dataset creation property list.
        try {
            dcpl_id = H5Pcreate(H5P_CLS_DATASET_CREATE_ID_g());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // set the external file.
        try {
            if (dcpl_id >= 0)
                H5Pset_external(dcpl_id, arena.allocateFrom(EXTERNALNAME), 0, H5F_UNLIMITED());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create the HDF5Constants.dataset.
        try {
            if ((file_id >= 0) && (filespace_id >= 0) && (dcpl_id >= 0))
                dataset_id = H5Dcreate2(file_id, arena.allocateFrom(DATASETNAME), H5T_STD_I32LE_g(),
                                        filespace_id, H5P_DEFAULT(), dcpl_id, H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Write the dataset.
        try {
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
            H5Dwrite(dataset_id, H5T_NATIVE_INT_g(), H5S_ALL(), H5S_ALL(), H5P_DEFAULT(), dataSeg);
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
            if (filespace_id >= 0)
                H5Sclose(filespace_id);
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

    private static void readExternal(Arena arena)
    {
        long file_id      = H5I_INVALID_HID();
        long dcpl_id      = H5I_INVALID_HID();
        long dataset_id   = H5I_INVALID_HID();
        int[][] dset_data = new int[DIM_X][DIM_Y];
        String[] Xname    = new String[1];

        // Open file using the default properties.
        try {
            file_id = H5Fopen(arena.allocateFrom(FILENAME), H5F_ACC_RDWR(), H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Open dataset using the default properties.
        try {
            if (file_id >= 0)
                dataset_id = H5Dopen2(file_id, arena.allocateFrom(DATASETNAME), H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Retrieve the dataset creation property list.
        try {
            if (dataset_id >= 0)
                dcpl_id = H5Dget_create_plist(dataset_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Retrieve and print the name of the external file.
        String externalFileName = "";
        try {
            if (dcpl_id >= 0) {
                MemorySegment namelenSeg = arena.allocate(ValueLayout.JAVA_LONG);
                MemorySegment nameSeg    = arena.allocate(256);
                MemorySegment offsetSeg  = arena.allocate(ValueLayout.JAVA_LONG);
                MemorySegment sizeSeg    = arena.allocate(ValueLayout.JAVA_LONG);
                H5Pget_external(dcpl_id, 0, 256, nameSeg, offsetSeg, sizeSeg);
                externalFileName = nameSeg.getString(0);
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }
        System.out.println(DATASETNAME + " is stored in file: " + externalFileName);

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
            H5Ex_D_External.writeExternal(arena);
            H5Ex_D_External.readExternal(arena);
        }
    }
}
