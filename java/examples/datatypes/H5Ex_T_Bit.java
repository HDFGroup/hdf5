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
  This example shows how to read and write bitfield
  datatypes to a dataset.  The program first writes bit
  fields to a dataset with a dataspace of DIM0xDIM1, then
  closes the file.  Next, it reopens the file, reads back
  the data, and outputs it to the screen.
 ************************************************************/

package examples.datatypes;

import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;

public class H5Ex_T_Bit {
    private static String FILENAME = "H5Ex_T_Bit.h5";
    private static String DATASETNAME = "DS1";
    private static final int DIM0 = 4;
    private static final int DIM1 = 7;
    private static final int RANK = 2;

    private static void CreateDataset() {
        long file_id = -1;
        long dataspace_id = -1;
        long dataset_id = -1;
        long[] dims = { DIM0, DIM1 };
        int[][] dset_data = new int[DIM0][DIM1];

        // Initialize data.
        for (int indx = 0; indx < DIM0; indx++)
            for (int jndx = 0; jndx < DIM1; jndx++) {
                dset_data[indx][jndx] = 0;
                dset_data[indx][jndx] |= (indx * jndx - jndx) & 0x03; /* Field "A" */
                dset_data[indx][jndx] |= (indx & 0x03) << 2; /* Field "B" */
                dset_data[indx][jndx] |= (jndx & 0x03) << 4; /* Field "C" */
                dset_data[indx][jndx] |= ((indx + jndx) & 0x03) << 6; /* Field "D" */
            }

        // Create a new file using default properties.
        try {
            file_id = H5.H5Fcreate(FILENAME, HDF5Constants.H5F_ACC_TRUNC, HDF5Constants.H5P_DEFAULT,
                    HDF5Constants.H5P_DEFAULT);
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

        // Create the dataset.
        try {
            if ((file_id >= 0) && (dataspace_id >= 0))
                dataset_id = H5.H5Dcreate(file_id, DATASETNAME, HDF5Constants.H5T_STD_B8BE, dataspace_id,
                        HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Write the bitfield data to the dataset.
        try {
            if (dataset_id >= 0)
                H5.H5Dwrite(dataset_id, HDF5Constants.H5T_NATIVE_B8, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
                        HDF5Constants.H5P_DEFAULT, dset_data);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // End access to the dataset and release resources used by it.
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
        long dataspace_id = -1;
        long dataset_id = -1;
        long[] dims = { DIM0, DIM1 };
        int[][] dset_data;

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

        // Get dataspace and allocate memory for read buffer.
        try {
            if (dataset_id >= 0)
                dataspace_id = H5.H5Dget_space(dataset_id);
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

        // Allocate array of pointers to two-dimensional arrays (the
        // elements of the dataset.
        dset_data = new int[(int) dims[0]][(int) (dims[1])];

        // Read data.
        try {
            if (dataset_id >= 0)
                H5.H5Dread(dataset_id, HDF5Constants.H5T_NATIVE_B8, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
                        HDF5Constants.H5P_DEFAULT, dset_data);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Output the data to the screen.
        System.out.println(DATASETNAME + ":");
        for (int indx = 0; indx < dims[0]; indx++) {
            System.out.print(" [");
            for (int jndx = 0; jndx < dims[1]; jndx++) {
                System.out.print("{" + (dset_data[indx][jndx] & 0x03) + ", ");
                System.out.print(((dset_data[indx][jndx] >> 2) & 0x03) + ", ");
                System.out.print(((dset_data[indx][jndx] >> 4) & 0x03) + ", ");
                System.out.print(((dset_data[indx][jndx] >> 6) & 0x03) + "}");
            }
            System.out.println("]");
        }
        System.out.println();

        // End access to the dataset and release resources used by it.
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
        H5Ex_T_Bit.CreateDataset();
        // Now we begin the read section of this example. Here we assume
        // the dataset and array have the same name and rank, but can have
        // any size. Therefore we must allocate a new array to read in
        // data using malloc().
        H5Ex_T_Bit.ReadDataset();
    }

}
