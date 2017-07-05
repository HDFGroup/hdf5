/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/************************************************************
    Creating a dataset attribute.
 ************************************************************/

package examples.intro;

import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;

public class H5_CreateAttribute {
    private static String FILENAME = "H5_CreateAttribute.h5";
    private static String DATASETNAME = "dset";
    private static final int DIM_X = 4;
    private static final int DIM_Y = 6;
    private static String DATASETATTRIBUTE = "Units";

    private static void CreateDatasetAttribute() {
        long file_id = -1;
        long dataspace_id = -1;
        long dataset_id = -1;
        long attribute_id = -1;
        long[] dims1 = { DIM_X, DIM_Y };
        long[] dims = { 2 };
        int[] attr_data = { 100, 200 };

        // Create a new file using default properties.
        try {
            file_id = H5.H5Fcreate(FILENAME, HDF5Constants.H5F_ACC_TRUNC, HDF5Constants.H5P_DEFAULT,
                    HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create the data space for the dataset.
        try {
            dataspace_id = H5.H5Screate_simple(2, dims1, null);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create the dataset.
        try {
            if ((file_id >= 0) && (dataspace_id >= 0))
                dataset_id = H5.H5Dcreate(file_id, "/" + DATASETNAME, HDF5Constants.H5T_STD_I32BE, dataspace_id,
                        HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Terminate access to the data space.
        try {
            if (dataspace_id >= 0)
                H5.H5Sclose(dataspace_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create the data space for the attribute.
        try {
            dataspace_id = H5.H5Screate_simple(1, dims, null);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create a dataset attribute.
        try {
            if ((dataset_id >= 0) && (dataspace_id >= 0))
                attribute_id = H5.H5Acreate(dataset_id, DATASETATTRIBUTE, HDF5Constants.H5T_STD_I32BE, dataspace_id,
                        HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Write the attribute data.
        try {
            if (attribute_id >= 0)
                H5.H5Awrite(attribute_id, HDF5Constants.H5T_NATIVE_INT, attr_data);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Close the attribute.
        try {
            if (attribute_id >= 0)
                H5.H5Aclose(attribute_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Close the dataspace.
        try {
            if (dataspace_id >= 0)
                H5.H5Sclose(dataspace_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Close to the dataset.
        try {
            if (dataset_id >= 0)
                H5.H5Dclose(dataset_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Close the file.
        try {
            if (file_id >= 0)
                H5.H5Fclose(file_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static void main(String[] args) {
        H5_CreateAttribute.CreateDatasetAttribute();
    }

}
