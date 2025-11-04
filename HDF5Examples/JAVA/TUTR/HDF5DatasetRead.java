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

/**
 * <p>
 * Title: HDF Native Package (Java) Example
 * </p>
 * <p>
 * Description: this example shows how to read/write HDF datasets using the
 * "HDF Native Package (Java)". The example creates an integer dataset, and read
 * and write data values:
 *
 * <pre>
 *     "/" (root)
 *             2D 32-bit integer 20x10
 * </pre>
 *
 * </p>
 */
public class HDF5DatasetRead {
    private static String fname  = "HDF5DatasetRead.h5";
    private static String dsname = "2D 32-bit integer 20x10";
    private static long[] dims2D = {20, 10};

    private static void ReadWriteDataset(Arena arena)
    {
        long file_id    = H5I_INVALID_HID();
        long dataset_id = H5I_INVALID_HID();

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

        // Open dataset using the default properties.
        try {
            if (file_id >= 0)
                dataset_id = H5Dopen2(file_id, arena.allocateFrom(dsname), H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Allocate array of pointers to two-dimensional arrays (the
        // elements of the dataset.
        int[][] dataRead = new int[(int)dims2D[0]][(int)(dims2D[1])];

        try {
            if (dataset_id >= 0) {
                // Allocate MemorySegment for reading
                int totalSize         = (int)dims2D[0] * (int)dims2D[1];
                MemorySegment readSeg = arena.allocate(ValueLayout.JAVA_INT, totalSize);
                H5Dread(dataset_id, H5T_NATIVE_INT_g(), H5S_ALL(), H5S_ALL(), H5P_DEFAULT(), readSeg);
                // Unflatten 1D MemorySegment to 2D array
                for (int i = 0; i < (int)dims2D[0]; i++)
                    for (int j = 0; j < (int)dims2D[1]; j++)
                        dataRead[i][j] = readSeg.getAtIndex(ValueLayout.JAVA_INT, i * (int)dims2D[1] + j);
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // print out the data values
        System.out.println("\n\nOriginal Data Values");
        for (int i = 0; i < 20; i++) {
            System.out.print("\n" + dataRead[i][0]);
            for (int j = 1; j < 10; j++) {
                System.out.print(", " + dataRead[i][j]);
            }
        }

        // change data value and write it to file.
        for (int i = 0; i < 20; i++) {
            for (int j = 0; j < 10; j++) {
                dataRead[i][j]++;
            }
        }

        // Write the data to the dataset.
        try {
            if (dataset_id >= 0) {
                // Flatten 2D array to 1D for MemorySegment
                int totalSize  = (int)dims2D[0] * (int)dims2D[1];
                int[] flatData = new int[totalSize];
                for (int i = 0; i < (int)dims2D[0]; i++)
                    for (int j = 0; j < (int)dims2D[1]; j++)
                        flatData[i * (int)dims2D[1] + j] = dataRead[i][j];
                MemorySegment writeSeg = arena.allocateFrom(ValueLayout.JAVA_INT, flatData);
                H5Dwrite(dataset_id, H5T_NATIVE_INT_g(), H5S_ALL(), H5S_ALL(), H5P_DEFAULT(), writeSeg);
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // reload the data value
        int[][] dataModified = new int[(int)dims2D[0]][(int)(dims2D[1])];

        try {
            if (dataset_id >= 0) {
                // Allocate MemorySegment for reading
                int totalSize             = (int)dims2D[0] * (int)dims2D[1];
                MemorySegment modifiedSeg = arena.allocate(ValueLayout.JAVA_INT, totalSize);
                H5Dread(dataset_id, H5T_NATIVE_INT_g(), H5S_ALL(), H5S_ALL(), H5P_DEFAULT(), modifiedSeg);
                // Unflatten 1D MemorySegment to 2D array
                for (int i = 0; i < (int)dims2D[0]; i++)
                    for (int j = 0; j < (int)dims2D[1]; j++)
                        dataModified[i][j] =
                            modifiedSeg.getAtIndex(ValueLayout.JAVA_INT, i * (int)dims2D[1] + j);
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // print out the modified data values
        System.out.println("\n\nModified Data Values");
        for (int i = 0; i < 20; i++) {
            System.out.print("\n" + dataModified[i][0]);
            for (int j = 1; j < 10; j++) {
                System.out.print(", " + dataModified[i][j]);
            }
        }

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

    /**
     * create the file and add groups ans dataset into the file, which is the
     * same as javaExample.H5DatasetCreate
     *
     * @see HDF5DatasetCreate.H5DatasetCreate
     * @throws Exception
     */
    private static void createFile(Arena arena) throws Exception
    {
        long file_id      = H5I_INVALID_HID();
        long dataspace_id = H5I_INVALID_HID();
        long dataset_id   = H5I_INVALID_HID();

        // Create a new file using default properties.
        try {
            file_id = H5Fcreate(arena.allocateFrom(fname), H5F_ACC_TRUNC(), H5P_DEFAULT(), H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create the data space for the dataset.
        try {
            dataspace_id =
                H5Screate_simple(2, arena.allocateFrom(ValueLayout.JAVA_LONG, dims2D), MemorySegment.NULL);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create the dataset.
        try {
            if ((file_id >= 0) && (dataspace_id >= 0))
                dataset_id = H5Dcreate2(file_id, arena.allocateFrom(dsname), H5T_STD_I32LE_g(), dataspace_id,
                                        H5P_DEFAULT(), H5P_DEFAULT(), H5P_DEFAULT());
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

        // set the data values
        int[] dataIn = new int[20 * 10];
        for (int i = 0; i < 20; i++) {
            for (int j = 0; j < 10; j++) {
                dataIn[i * 10 + j] = i * 100 + j;
            }
        }

        // Write the data to the dataset.
        try {
            if (dataset_id >= 0) {
                MemorySegment dataSeg = arena.allocateFrom(ValueLayout.JAVA_INT, dataIn);
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
            HDF5DatasetRead.ReadWriteDataset(arena);
        }
    }
}
