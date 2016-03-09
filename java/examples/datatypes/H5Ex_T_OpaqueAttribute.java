/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/************************************************************
  This example shows how to read and write opaque datatypes
  to an attribute.  The program first writes opaque data to
  an attribute with a dataspace of DIM0, then closes the
  file. Next, it reopens the file, reads back the data, and
  outputs it to the screen.
 ************************************************************/

package examples.datatypes;

import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;

public class H5Ex_T_OpaqueAttribute {
    private static String FILENAME = "H5Ex_T_OpaqueAttribute.h5";
    private static String DATASETNAME = "DS1";
    private static String ATTRIBUTENAME = "A1";
    private static final int DIM0 = 4;
    private static final int LEN = 7;
    private static final int RANK = 1;

    private static void CreateDataset() {
        long file_id = -1;
        long dataspace_id = -1;
        long datatype_id = -1;
        long dataset_id = -1;
        long attribute_id = -1;
        long[] dims = { DIM0 };
        byte[] dset_data = new byte[DIM0 * LEN];
        byte[] str_data = { 'O', 'P', 'A', 'Q', 'U', 'E' };

        // Initialize data.
        for (int indx = 0; indx < DIM0; indx++) {
            for (int jndx = 0; jndx < LEN - 1; jndx++)
                dset_data[jndx + indx * LEN] = str_data[jndx];
            dset_data[LEN - 1 + indx * LEN] = (byte) (indx + '0');
        }

        // Create a new file using default properties.
        try {
            file_id = H5.H5Fcreate(FILENAME, HDF5Constants.H5F_ACC_TRUNC, HDF5Constants.H5P_DEFAULT,
                    HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create dataset with a scalar dataspace.
        try {
            dataspace_id = H5.H5Screate(HDF5Constants.H5S_SCALAR);
            if (dataspace_id >= 0) {
                dataset_id = H5.H5Dcreate(file_id, DATASETNAME, HDF5Constants.H5T_STD_I32LE, dataspace_id,
                        HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
                H5.H5Sclose(dataspace_id);
                dataspace_id = -1;
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create opaque datatype and set the tag to something appropriate.
        // For this example we will write and view the data as a character
        // array.
        try {
            datatype_id = H5.H5Tcreate(HDF5Constants.H5T_OPAQUE, (long)LEN);
            if (datatype_id >= 0)
                H5.H5Tset_tag(datatype_id, "Character array");
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create dataspace. Setting maximum size to NULL sets the maximum
        // size to be the current size.
        try {
            dataspace_id = H5.H5Screate_simple(RANK, dims, null);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create the attribute and write the array data to it.
        try {
            if ((dataset_id >= 0) && (datatype_id >= 0) && (dataspace_id >= 0))
                attribute_id = H5.H5Acreate(dataset_id, ATTRIBUTENAME, datatype_id, dataspace_id,
                        HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Write the dataset.
        try {
            if ((attribute_id >= 0) && (datatype_id >= 0))
                H5.H5Awrite(attribute_id, datatype_id, dset_data);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // End access to the dataset and release resources used by it.
        try {
            if (attribute_id >= 0)
                H5.H5Aclose(attribute_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        try {
            if (dataset_id >= 0)
                H5.H5Dclose(dataset_id);
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

        try {
            if (datatype_id >= 0)
                H5.H5Tclose(datatype_id);
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

    private static void ReadDataset() {
        long file_id = -1;
        long datatype_id = -1;
        long dataspace_id = -1;
        long dataset_id = -1;
        long attribute_id = -1;
        long type_len = -1;
        long[] dims = { DIM0 };
        byte[] dset_data;
        String tag_name = null;

        // Open an existing file.
        try {
            file_id = H5.H5Fopen(FILENAME, HDF5Constants.H5F_ACC_RDONLY, HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Open an existing dataset.
        try {
            if (file_id >= 0)
                dataset_id = H5.H5Dopen(file_id, DATASETNAME, HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        try {
            if (dataset_id >= 0)
                attribute_id = H5.H5Aopen_by_name(dataset_id, ".", ATTRIBUTENAME, HDF5Constants.H5P_DEFAULT,
                        HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Get datatype and properties for the datatype.
        try {
            if (attribute_id >= 0)
                datatype_id = H5.H5Aget_type(attribute_id);
            if (datatype_id >= 0) {
                type_len = H5.H5Tget_size(datatype_id);
                tag_name = H5.H5Tget_tag(datatype_id);
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Get dataspace and allocate memory for read buffer.
        try {
            if (attribute_id >= 0)
                dataspace_id = H5.H5Aget_space(attribute_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        try {
            if (dataspace_id >= 0)
                H5.H5Sget_simple_extent_dims(dataspace_id, dims, null);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Allocate buffer.
        dset_data = new byte[(int) (dims[0] * type_len)];

        // Read data.
        try {
            if ((attribute_id >= 0) && (datatype_id >= 0))
                H5.H5Aread(attribute_id, datatype_id, dset_data);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Output the data to the screen.
        System.out.println("Datatype tag for " + ATTRIBUTENAME + " is: \"" + tag_name + "\"");
        for (int indx = 0; indx < dims[0]; indx++) {
            System.out.print(ATTRIBUTENAME + "[" + indx + "]: ");
            for (int jndx = 0; jndx < type_len; jndx++) {
                char temp = (char) dset_data[jndx + indx * (int)type_len];
                System.out.print(temp);
            }
            System.out.println("");
        }
        System.out.println();

        // End access to the dataset and release resources used by it.
        try {
            if (attribute_id >= 0)
                H5.H5Aclose(attribute_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        try {
            if (dataset_id >= 0)
                H5.H5Dclose(dataset_id);
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

        try {
            if (datatype_id >= 0)
                H5.H5Tclose(datatype_id);
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
        H5Ex_T_OpaqueAttribute.CreateDataset();
        // Now we begin the read section of this example. Here we assume
        // the dataset and array have the same name and rank, but can have
        // any size. Therefore we must allocate a new array to read in
        // data using malloc().
        H5Ex_T_OpaqueAttribute.ReadDataset();
    }

}
