/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/************************************************************
    Creating and writing a VL string to a file.
 ************************************************************/

import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;

public class H5Ex_T_VLString {
    private static String FILENAME    = "H5Ex_T_VLString.h5";
    private static String DATASETNAME = "DS1";

    private static void createDataset()
    {
        long file_id      = HDF5Constants.H5I_INVALID_HID;
        long type_id      = HDF5Constants.H5I_INVALID_HID;
        long dataspace_id = HDF5Constants.H5I_INVALID_HID;
        long dataset_id   = HDF5Constants.H5I_INVALID_HID;
        int rank          = 1;
        String[] str_data = {"Parting", "is such", "sweet", "sorrow."};
        long[] dims       = {str_data.length};

        // Create a new file using default properties.
        try {
            file_id = H5.H5Fcreate(FILENAME, HDF5Constants.H5F_ACC_TRUNC, HDF5Constants.H5P_DEFAULT,
                                   HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        try {
            type_id = H5.H5Tcopy(HDF5Constants.H5T_C_S1);
            H5.H5Tset_size(type_id, HDF5Constants.H5T_VARIABLE);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create dataspace. Setting maximum size to NULL sets the maximum
        // size to be the current size.
        try {
            dataspace_id = H5.H5Screate_simple(rank, dims, null);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create the dataset and write the string data to it.
        try {
            if ((file_id >= 0) && (type_id >= 0) && (dataspace_id >= 0)) {
                dataset_id =
                    H5.H5Dcreate(file_id, DATASETNAME, type_id, dataspace_id, HDF5Constants.H5P_DEFAULT,
                                 HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Write the data to the dataset.
        try {
            if (dataset_id >= 0)
                H5.H5DwriteVL(dataset_id, type_id, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
                              HDF5Constants.H5P_DEFAULT, str_data);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        try {
            H5.H5Sclose(dataspace_id);
            H5.H5Tclose(type_id);
            H5.H5Dclose(dataset_id);
            H5.H5Fclose(file_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }
    }

    private static void readDataset()
    {
        long file_id      = HDF5Constants.H5I_INVALID_HID;
        long type_id      = HDF5Constants.H5I_INVALID_HID;
        long dataset_id   = HDF5Constants.H5I_INVALID_HID;
        String[] str_data = {"", "", "", ""};

        try {
            file_id = H5.H5Fopen(FILENAME, HDF5Constants.H5F_ACC_RDONLY, HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        try {
            dataset_id = H5.H5Dopen(file_id, DATASETNAME, HDF5Constants.H5P_DEFAULT);
            type_id    = H5.H5Dget_type(dataset_id);
            H5.H5DreadVL(dataset_id, type_id, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
                         HDF5Constants.H5P_DEFAULT, str_data);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        for (int indx = 0; indx < str_data.length; indx++)
            System.out.println(DATASETNAME + " [" + indx + "]: " + str_data[indx]);

        try {
            H5.H5Tclose(type_id);
            H5.H5Dclose(dataset_id);
            H5.H5Fclose(file_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static void main(String[] args)
    {
        H5Ex_T_VLString.createDataset();
        H5Ex_T_VLString.readDataset();
    }
}
