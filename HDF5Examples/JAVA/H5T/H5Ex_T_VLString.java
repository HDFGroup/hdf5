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
    Creating and writing a VL string to a file.
 ************************************************************/

import static org.hdfgroup.javahdf5.hdf5_h.*;

import java.lang.foreign.Arena;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.ValueLayout;

import hdf.hdf5lib.H5;

public class H5Ex_T_VLString {
    private static String FILENAME    = "H5Ex_T_VLString.h5";
    private static String DATASETNAME = "DS1";

    private static void createDataset(Arena arena)
    {
        long file_id      = H5I_INVALID_HID();
        long type_id      = H5I_INVALID_HID();
        long dataspace_id = H5I_INVALID_HID();
        long dataset_id   = H5I_INVALID_HID();
        int rank          = 1;
        String[] str_data = {"Parting", "is such", "sweet", "sorrow."};
        long[] dims       = {str_data.length};

        // Create a new file using default properties.
        try {
            file_id = H5Fcreate(arena.allocateFrom(FILENAME), H5F_ACC_TRUNC(), H5P_DEFAULT(), H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        try {
            type_id = H5Tcopy(H5T_C_S1_g());
            H5Tset_size(type_id, H5T_VARIABLE());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create dataspace. Setting maximum size to NULL sets the maximum
        // size to be the current size.
        try {
            dataspace_id =
                H5Screate_simple(rank, arena.allocateFrom(ValueLayout.JAVA_LONG, dims), MemorySegment.NULL);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create the dataset and write the string data to it.
        try {
            if ((file_id >= 0) && (type_id >= 0) && (dataspace_id >= 0)) {
                dataset_id = H5Dcreate2(file_id, arena.allocateFrom(DATASETNAME), type_id, dataspace_id,
                                        H5P_DEFAULT(), H5P_DEFAULT(), H5P_DEFAULT());
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Write the data to the dataset.
        try {
            if (dataset_id >= 0)
                H5.H5DwriteVL(dataset_id, type_id, H5S_ALL(), H5S_ALL(), H5P_DEFAULT(), str_data);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        try {
            H5Sclose(dataspace_id);
            H5Tclose(type_id);
            H5Dclose(dataset_id);
            H5Fclose(file_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }
    }

    private static void readDataset(Arena arena)
    {
        long file_id      = H5I_INVALID_HID();
        long type_id      = H5I_INVALID_HID();
        long dataset_id   = H5I_INVALID_HID();
        String[] str_data = {"", "", "", ""};

        try {
            file_id = H5Fopen(arena.allocateFrom(FILENAME), H5F_ACC_RDONLY(), H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        try {
            dataset_id = H5Dopen2(file_id, arena.allocateFrom(DATASETNAME), H5P_DEFAULT());
            type_id    = H5Dget_type(dataset_id);
            H5.H5DreadVL(dataset_id, type_id, H5S_ALL(), H5S_ALL(), H5P_DEFAULT(), str_data);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        for (int indx = 0; indx < str_data.length; indx++)
            System.out.println(DATASETNAME + " [" + indx + "]: " + str_data[indx]);

        try {
            H5Tclose(type_id);
            H5Dclose(dataset_id);
            H5Fclose(file_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static void main(String[] args)
    {

        try (Arena arena = Arena.ofConfined()) {
            H5Ex_T_VLString.createDataset(arena);
            H5Ex_T_VLString.readDataset(arena);
        }
    }
}
