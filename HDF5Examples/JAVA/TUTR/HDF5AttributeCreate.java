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
import static org.hdfgroup.javahdf5.hdf5_h_1.*;
import static org.hdfgroup.javahdf5.hdf5_h_2.*;

import java.lang.foreign.Arena;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.ValueLayout;



/**
 * <p>
 * Title: HDF Native Package (Java) Example
 * </p>
 * <p>
 * Description: this example shows how to create/read/write HDF attribute using
 * the "HDF Native Package (Java)". The example creates an attribute and, read
 * and write the attribute value:
 *
 * <pre>
 *     "/" (root)
 *             2D 32-bit integer 20x10
 *             (attribute: name="data range", value=[0, 10000])
 * </pre>
 *
 * </p>
 */
public class HDF5AttributeCreate {
    private static String fname    = "HDF5AttributeCreate.h5";
    private static String dsname   = "2D 32-bit integer 20x10";
    private static String attrname = "data range";
    private static long[] dims2D   = {20, 10};

    private static void CreateDatasetAttribute(Arena arena)
    {
        long file_id      = H5I_INVALID_HID();
        long dataspace_id = H5I_INVALID_HID();
        long dataset_id   = H5I_INVALID_HID();
        long attribute_id = H5I_INVALID_HID();

        // create the file and add groups and dataset into the file
        try {
            createFile();
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Open file using the default properties.
        try {
            file_id = H5Fopen(fname, H5F_ACC_RDWR(), H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Open dataset using the default properties.
        try {
            if (file_id >= 0)
                dataset_id = H5Dopen2(file_id, dsname, H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        long[] attrDims = {2};        // 1D of size two
        int[] attrValue = {0, 10000}; // attribute value

        // Create the data space for the attribute.
        try {
            dataspace_id = H5Screate_simple(1, attrDims, null);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create a dataset attribute.
        try {
            if ((dataset_id >= 0) && (dataspace_id >= 0))
                attribute_id = H5Acreate(dataset_id, attrname, H5T_STD_I32BE_g(), dataspace_id,
                                            H5P_DEFAULT(), H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }
        // Write the attribute data.
        try {
            if (attribute_id >= 0)
                H5Awrite(attribute_id, H5T_NATIVE_INT_g(), attrValue);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Close the attribute.
        try {
            if (attribute_id >= 0)
                H5Aclose(attribute_id);
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
            if (dataset_id >= 0)
                attribute_id = H5Aopen_by_name(dataset_id, ".", attrname, H5P_DEFAULT(),
                                                  H5P_DEFAULT());
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
            if (dataspace_id >= 0)
                H5Sget_simple_extent_dims(dataspace_id, attrDims, null);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Allocate array of pointers to two-dimensional arrays (the
        // elements of the dataset.
        int[] attrData = new int[(int)attrDims[0]];

        // Read data.
        try {
            if (attribute_id >= 0)
                H5Aread(attribute_id, H5T_NATIVE_INT_g(), attrData);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // print out attribute value
        System.out.println(attrname);
        System.out.println(attrData[0] + "  " + attrData[1]);

        // Close the dataspace.
        try {
            if (dataspace_id >= 0)
                H5Sclose(dataspace_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Close to the dataset.
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
     * create the file and add groups and dataset into the file, which is the
     * same as javaExample.H5DatasetCreate
     *
     * @see javaExample.HDF5DatasetCreate
     * @throws Exception
     */
    private static void createFile(Arena arena) throws Exception
    {
        long file_id      = H5I_INVALID_HID();
        long dataspace_id = H5I_INVALID_HID();
        long dataset_id   = H5I_INVALID_HID();

        // Create a new file using default properties.
        try {
            file_id = H5Fcreate(fname, H5F_ACC_TRUNC(), H5P_DEFAULT(),
                                   H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create the data space for the dataset.
        try {
            dataspace_id = H5Screate_simple(2, dims2D, null);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create the dataset.
        try {
            if ((file_id >= 0) && (dataspace_id >= 0))
                dataset_id = H5Dcreate2(file_id, dsname, H5T_STD_I32LE_g(), dataspace_id,
                                          H5P_DEFAULT(), H5P_DEFAULT(),
                                          H5P_DEFAULT());
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
            if (dataset_id >= 0)
                H5Dwrite(dataset_id, H5T_NATIVE_INT_g(), H5S_ALL(),
                            H5S_ALL(), H5P_DEFAULT(), dataIn);
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

    public static void main(String[] args) { HDF5AttributeCreate.CreateDatasetAttribute(); }
}
