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
  This example shows how to read and write array datatypes
  to an attribute.  The program first writes integers arrays
  of dimension ADIM0xADIM1 to an attribute with a dataspace
  of DIM0, then closes the  file.  Next, it reopens the
  file, reads back the data, and outputs it to the screen.
 ************************************************************/

import static org.hdfgroup.javahdf5.hdf5_h.*;

import java.lang.foreign.Arena;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.ValueLayout;

public class H5Ex_T_ArrayAttribute {
    private static String FILENAME      = "H5Ex_T_ArrayAttribute.h5";
    private static String DATASETNAME   = "DS1";
    private static String ATTRIBUTENAME = "A1";
    private static final int DIM0       = 4;
    private static final int ADIM0      = 3;
    private static final int ADIM1      = 5;
    private static final int RANK       = 1;
    private static final int NDIMS      = 2;

    private static void CreateDataset(Arena arena)
    {
        long file_id        = H5I_INVALID_HID();
        long filetype_id    = H5I_INVALID_HID();
        long memtype_id     = H5I_INVALID_HID();
        long dataspace_id   = H5I_INVALID_HID();
        long dataset_id     = H5I_INVALID_HID();
        long attribute_id   = H5I_INVALID_HID();
        long[] dims         = {DIM0};
        long[] adims        = {ADIM0, ADIM1};
        int[][][] dset_data = new int[DIM0][ADIM0][ADIM1];

        // Initialize data. indx is the element in the dataspace, jndx and kndx the
        // elements within the array datatype.
        for (int indx = 0; indx < DIM0; indx++)
            for (int jndx = 0; jndx < ADIM0; jndx++)
                for (int kndx = 0; kndx < ADIM1; kndx++)
                    dset_data[indx][jndx][kndx] = indx * jndx - jndx * kndx + indx * kndx;

        // Create a new file using default properties.
        try {
            file_id = H5Fcreate(arena.allocateFrom(FILENAME), H5F_ACC_TRUNC(), H5P_DEFAULT(), H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create array datatypes for file.
        try {
            filetype_id =
                H5Tarray_create2(H5T_STD_I64LE_g(), NDIMS, arena.allocateFrom(ValueLayout.JAVA_LONG, adims));
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create array datatypes for memory.
        try {
            memtype_id =
                H5Tarray_create2(H5T_NATIVE_INT_g(), NDIMS, arena.allocateFrom(ValueLayout.JAVA_LONG, adims));
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create dataset with a scalar dataspace.
        try {
            dataspace_id = H5Screate(H5S_SCALAR());
            if (dataspace_id >= 0) {
                dataset_id = H5Dcreate2(file_id, arena.allocateFrom(DATASETNAME), H5T_STD_I32LE_g(),
                                        dataspace_id, H5P_DEFAULT(), H5P_DEFAULT(), H5P_DEFAULT());
                H5Sclose(dataspace_id);
                dataspace_id = H5I_INVALID_HID();
            }
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

        // Create the attribute and write the array data to it.
        try {
            if ((dataset_id >= 0) && (dataspace_id >= 0) && (filetype_id >= 0))
                attribute_id = H5Acreate2(dataset_id, arena.allocateFrom(ATTRIBUTENAME), filetype_id,
                                          dataspace_id, H5P_DEFAULT(), H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Write the dataset.
        try {
            if ((attribute_id >= 0) && (memtype_id >= 0)) {
                // Flatten the 3D array to 1D for MemorySegment
                int totalSize  = DIM0 * ADIM0 * ADIM1;
                int[] flatData = new int[totalSize];
                for (int i = 0; i < DIM0; i++) {
                    for (int j = 0; j < ADIM0; j++) {
                        for (int k = 0; k < ADIM1; k++) {
                            flatData[i * ADIM0 * ADIM1 + j * ADIM1 + k] = dset_data[i][j][k];
                        }
                    }
                }
                MemorySegment dataSeg = arena.allocateFrom(ValueLayout.JAVA_INT, flatData);
                H5Awrite(attribute_id, memtype_id, dataSeg);
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // End access to the dataset and release resources used by it.
        try {
            if (attribute_id >= 0)
                H5Aclose(attribute_id);
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

        // Terminate access to the data space.
        try {
            if (dataspace_id >= 0)
                H5Sclose(dataspace_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Terminate access to the file type.
        try {
            if (filetype_id >= 0)
                H5Tclose(filetype_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Terminate access to the mem type.
        try {
            if (memtype_id >= 0)
                H5Tclose(memtype_id);
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
        long filetype_id  = H5I_INVALID_HID();
        long memtype_id   = H5I_INVALID_HID();
        long dataset_id   = H5I_INVALID_HID();
        long attribute_id = H5I_INVALID_HID();
        long[] dims       = {DIM0};
        long[] adims      = {ADIM0, ADIM1};
        int[][][] dset_data;

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

        try {
            if (dataset_id >= 0)
                attribute_id =
                    H5Aopen_by_name(dataset_id, arena.allocateFrom("."), arena.allocateFrom(ATTRIBUTENAME),
                                    H5P_DEFAULT(), H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Get the datatype.
        try {
            if (attribute_id >= 0)
                filetype_id = H5Aget_type(attribute_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Get the datatype's dimensions.
        try {
            if (filetype_id >= 0) {
                MemorySegment adimsSeg = arena.allocateFrom(ValueLayout.JAVA_LONG, adims);
                H5Tget_array_dims2(filetype_id, adimsSeg);
                // Read back the dimensions
                for (int i = 0; i < adims.length; i++) {
                    adims[i] = adimsSeg.getAtIndex(ValueLayout.JAVA_LONG, i);
                }
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Allocate array of pointers to two-dimensional arrays (the
        // elements of the dataset.
        dset_data = new int[(int)dims[0]][(int)(adims[0])][(int)(adims[1])];

        // Create array datatypes for memory.
        try {
            memtype_id =
                H5Tarray_create2(H5T_NATIVE_INT_g(), 2, arena.allocateFrom(ValueLayout.JAVA_LONG, adims));
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Read data.
        try {
            if ((attribute_id >= 0) && (memtype_id >= 0)) {
                int totalSize         = (int)(dims[0] * adims[0] * adims[1]);
                MemorySegment dataSeg = arena.allocate(ValueLayout.JAVA_INT, totalSize);
                H5Aread(attribute_id, memtype_id, dataSeg);
                // Unflatten the 1D MemorySegment to 3D array
                for (int i = 0; i < dims[0]; i++) {
                    for (int j = 0; j < adims[0]; j++) {
                        for (int k = 0; k < adims[1]; k++) {
                            dset_data[i][j][k] = dataSeg.getAtIndex(
                                ValueLayout.JAVA_INT, (int)(i * adims[0] * adims[1] + j * adims[1] + k));
                        }
                    }
                }
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Output the data to the screen.
        for (int indx = 0; indx < dims[0]; indx++) {
            System.out.println(ATTRIBUTENAME + " [" + indx + "]:");
            for (int jndx = 0; jndx < adims[0]; jndx++) {
                System.out.print(" [");
                for (int kndx = 0; kndx < adims[1]; kndx++)
                    System.out.print(dset_data[indx][jndx][kndx] + " ");
                System.out.println("]");
            }
            System.out.println();
        }
        System.out.println();

        // End access to the dataset and release resources used by it.
        try {
            if (attribute_id >= 0)
                H5Aclose(attribute_id);
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

        // Terminate access to the file type.
        try {
            if (filetype_id >= 0)
                H5Tclose(filetype_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Terminate access to the mem type.
        try {
            if (memtype_id >= 0)
                H5Tclose(memtype_id);
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
            H5Ex_T_ArrayAttribute.CreateDataset(arena);
            H5Ex_T_ArrayAttribute.ReadDataset(arena);
        }
    }
}
