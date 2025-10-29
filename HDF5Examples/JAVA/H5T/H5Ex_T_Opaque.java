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
  This example shows how to read and write opaque datatypes
  to a dataset.  The program first writes opaque data to a
  dataset with a dataspace of DIM0, then closes the file.
  Next, it reopens the file, reads back the data, and
  outputs it to the screen.
 ************************************************************/

import static org.hdfgroup.javahdf5.hdf5_h.*;

import java.lang.foreign.Arena;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.ValueLayout;

public class H5Ex_T_Opaque {
    private static String FILENAME    = "H5Ex_T_Opaque.h5";
    private static String DATASETNAME = "DS1";
    private static final int DIM0     = 4;
    private static final int LEN      = 7;
    private static final int RANK     = 1;

    private static void CreateDataset(Arena arena)
    {
        long file_id      = H5I_INVALID_HID();
        long dataspace_id = H5I_INVALID_HID();
        long datatype_id  = H5I_INVALID_HID();
        long dataset_id   = H5I_INVALID_HID();
        long[] dims       = {DIM0};
        byte[] dset_data  = new byte[DIM0 * LEN];
        byte[] str_data   = {'O', 'P', 'A', 'Q', 'U', 'E'};

        // Initialize data.
        for (int indx = 0; indx < DIM0; indx++) {
            for (int jndx = 0; jndx < LEN - 1; jndx++)
                dset_data[jndx + indx * LEN] = str_data[jndx];
            dset_data[LEN - 1 + indx * LEN] = (byte)(indx + '0');
        }

        // Create a new file using default properties.
        try {
            file_id = H5Fcreate(arena.allocateFrom(FILENAME), H5F_ACC_TRUNC(), H5P_DEFAULT(), H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create opaque datatype and set the tag to something appropriate.
        // For this example we will write and view the data as a character
        // array.
        try {
            datatype_id = H5Tcreate(H5T_OPAQUE(), (long)LEN);
            if (datatype_id >= 0)
                H5Tset_tag(datatype_id, arena.allocateFrom("Character array"));
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

        // Create the dataset and write the integer data to it. In this
        // example we will save the data as 64 bit big endian integers,
        // regardless of the native integer type. The HDF5 library
        // automatically converts between different integer types.
        try {
            if ((file_id >= 0) && (datatype_id >= 0) && (dataspace_id >= 0))
                dataset_id = H5Dcreate2(file_id, arena.allocateFrom(DATASETNAME), datatype_id, dataspace_id,
                                        H5P_DEFAULT(), H5P_DEFAULT(), H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Write the opaque data to the dataset.
        try {
            if ((dataset_id >= 0) && (datatype_id >= 0)) {
                MemorySegment dataSeg = arena.allocateFrom(ValueLayout.JAVA_BYTE, dset_data);
                H5Dwrite(dataset_id, datatype_id, H5S_ALL(), H5S_ALL(), H5P_DEFAULT(), dataSeg);
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

        try {
            if (datatype_id >= 0)
                H5Tclose(datatype_id);
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
        long datatype_id  = H5I_INVALID_HID();
        long dataspace_id = H5I_INVALID_HID();
        long dataset_id   = H5I_INVALID_HID();
        long type_len     = H5I_INVALID_HID();
        long[] dims       = {DIM0};
        byte[] dset_data;
        String tag_name = null;

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

        // Get datatype and properties for the datatype.
        try {
            if (dataset_id >= 0)
                datatype_id = H5Dget_type(dataset_id);
            if (datatype_id >= 0) {
                type_len             = H5Tget_size(datatype_id);
                MemorySegment tagSeg = H5Tget_tag(datatype_id);
                tag_name             = tagSeg.getString(0);
            }
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

        // Allocate buffer.
        dset_data = new byte[(int)(dims[0] * type_len)];

        // Read data.
        try {
            if ((dataset_id >= 0) && (datatype_id >= 0)) {
                MemorySegment dataSeg = arena.allocate(ValueLayout.JAVA_BYTE, dset_data.length);
                H5Dread(dataset_id, datatype_id, H5S_ALL(), H5S_ALL(), H5P_DEFAULT(), dataSeg);
                // Copy from MemorySegment back to byte array
                for (int i = 0; i < dset_data.length; i++) {
                    dset_data[i] = dataSeg.get(ValueLayout.JAVA_BYTE, i);
                }
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Output the data to the screen.
        System.out.println("Datatype tag for " + DATASETNAME + " is: \"" + tag_name + "\"");
        for (int indx = 0; indx < dims[0]; indx++) {
            System.out.print(DATASETNAME + "[" + indx + "]: ");
            for (int jndx = 0; jndx < type_len; jndx++) {
                char temp = (char)dset_data[jndx + indx * (int)type_len];
                System.out.print(temp);
            }
            System.out.println("");
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

        try {
            if (datatype_id >= 0)
                H5Tclose(datatype_id);
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
            H5Ex_T_Opaque.CreateDataset(arena);
            H5Ex_T_Opaque.ReadDataset(arena);
        }
    }
}
