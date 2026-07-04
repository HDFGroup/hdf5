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
  This example shows how to read and write string datatypes
  to an attribute.  The program first writes strings to an
  attribute with a dataspace of DIM0, then closes the file.
  Next, it reopens the file, reads back the data, and
  outputs it to the screen.
 ************************************************************/

import static org.hdfgroup.javahdf5.hdf5_h.*;

import java.lang.foreign.Arena;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.ValueLayout;

public class H5Ex_T_StringAttribute {
    private static String FILENAME      = "H5Ex_T_StringAttribute.h5";
    private static String DATASETNAME   = "DS1";
    private static String ATTRIBUTENAME = "A1";
    private static final int DIM0       = 4;
    private static final int SDIM       = 8;
    private static final int RANK       = 1;

    private static void CreateDataset(Arena arena)
    {
        long file_id            = H5I_INVALID_HID();
        long memtype_id         = H5I_INVALID_HID();
        long filetype_id        = H5I_INVALID_HID();
        long dataspace_id       = H5I_INVALID_HID();
        long dataset_id         = H5I_INVALID_HID();
        long attribute_id       = H5I_INVALID_HID();
        long[] dims             = {DIM0};
        byte[][] dset_data      = new byte[DIM0][SDIM];
        StringBuffer[] str_data = {new StringBuffer("Parting"), new StringBuffer("is such"),
                                   new StringBuffer("sweet"), new StringBuffer("sorrow.")};

        // Create a new file using default properties.
        try {
            file_id = H5Fcreate(arena.allocateFrom(FILENAME), H5F_ACC_TRUNC(), H5P_DEFAULT(), H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create file and memory datatypes. For this example we will save
        // the strings as FORTRAN strings, therefore they do not need space
        // for the null terminator in the file.
        try {
            filetype_id = H5Tcopy(H5T_FORTRAN_S1_g());
            if (filetype_id >= 0)
                H5Tset_size(filetype_id, SDIM - 1);
        }
        catch (Exception e) {
            e.printStackTrace();
        }
        try {
            memtype_id = H5Tcopy(H5T_C_S1_g());
            if (memtype_id >= 0)
                H5Tset_size(memtype_id, SDIM);
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

        // Create the attribute.
        try {
            if ((dataset_id >= 0) && (dataspace_id >= 0) && (filetype_id >= 0))
                attribute_id = H5Acreate2(dataset_id, arena.allocateFrom(ATTRIBUTENAME), filetype_id,
                                          dataspace_id, H5P_DEFAULT(), H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Write the data to the dataset.
        try {
            for (int indx = 0; indx < DIM0; indx++) {
                for (int jndx = 0; jndx < SDIM; jndx++) {
                    if (jndx < str_data[indx].length())
                        dset_data[indx][jndx] = (byte)str_data[indx].charAt(jndx);
                    else
                        dset_data[indx][jndx] = 0;
                }
            }
            if ((attribute_id >= 0) && (memtype_id >= 0)) {
                // Flatten 2D byte array to 1D for MemorySegment
                byte[] flatData = new byte[DIM0 * SDIM];
                for (int i = 0; i < DIM0; i++) {
                    for (int j = 0; j < SDIM; j++) {
                        flatData[i * SDIM + j] = dset_data[i][j];
                    }
                }
                MemorySegment dataSeg = arena.allocateFrom(ValueLayout.JAVA_BYTE, flatData);
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
        long dataspace_id = H5I_INVALID_HID();
        long dataset_id   = H5I_INVALID_HID();
        long attribute_id = H5I_INVALID_HID();
        long sdim         = 0;
        long[] dims       = {DIM0};
        byte[][] dset_data;
        StringBuffer[] str_data;

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

        // Get the datatype and its size.
        try {
            if (attribute_id >= 0)
                filetype_id = H5Aget_type(attribute_id);
            if (filetype_id >= 0) {
                sdim = H5Tget_size(filetype_id);
                sdim++; // Make room for null terminator
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Get dataspace and allocate memory for read buffer.
        try {
            if (attribute_id >= 0)
                dataspace_id = H5Aget_space(attribute_id);
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

        // Allocate space for data.
        dset_data = new byte[(int)dims[0]][(int)sdim];
        str_data  = new StringBuffer[(int)dims[0]];

        // Create the memory datatype.
        try {
            memtype_id = H5Tcopy(H5T_C_S1_g());
            if (memtype_id >= 0)
                H5Tset_size(memtype_id, sdim);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Read data.
        try {
            if ((attribute_id >= 0) && (memtype_id >= 0)) {
                int totalSize         = (int)dims[0] * (int)sdim;
                MemorySegment dataSeg = arena.allocate(ValueLayout.JAVA_BYTE, totalSize);
                H5Aread(attribute_id, memtype_id, dataSeg);
                // Unflatten 1D MemorySegment to 2D byte array
                for (int i = 0; i < (int)dims[0]; i++) {
                    for (int j = 0; j < sdim; j++) {
                        dset_data[i][j] = dataSeg.get(ValueLayout.JAVA_BYTE, i * sdim + j);
                    }
                }
            }
            byte[] tempbuf = new byte[(int)sdim];
            for (int indx = 0; indx < (int)dims[0]; indx++) {
                for (int jndx = 0; jndx < sdim; jndx++) {
                    tempbuf[jndx] = dset_data[indx][jndx];
                }
                str_data[indx] = new StringBuffer(new String(tempbuf).trim());
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Output the data to the screen.
        for (int indx = 0; indx < dims[0]; indx++) {
            System.out.println(DATASETNAME + " [" + indx + "]: " + str_data[indx]);
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

    public static void main(String[] args)
    {

        try (Arena arena = Arena.ofConfined()) {
            H5Ex_T_StringAttribute.CreateDataset(arena);
            H5Ex_T_StringAttribute.ReadDataset(arena);
        }
    }
}
