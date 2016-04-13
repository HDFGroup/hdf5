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
  This example shows how to create and extend an unlimited
  dataset.  The program first writes integers to a dataset
  with dataspace dimensions of DIM_XxDIM_Y, then closes the
  file.  Next, it reopens the file, reads back the data,
  outputs it to the screen, extends the dataset, and writes
  new data to the extended portions of the dataset.  Finally
  it reopens the file again, reads back the data, and
  outputs it to the screen.
 ************************************************************/
package examples.datasets;

import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;

public class H5Ex_D_UnlimitedAdd {
    private static String FILENAME = "H5Ex_D_UnlimitedAdd.h5";
    private static String DATASETNAME = "DS1";
    private static final int DIM_X = 4;
    private static final int DIM_Y = 7;
    private static final int EDIM_X = 6;
    private static final int EDIM_Y = 10;
    private static final int CHUNK_X = 4;
    private static final int CHUNK_Y = 4;
    private static final int RANK = 2;
    private static final int NDIMS = 2;

    private static void writeUnlimited() {
        long file_id = -1;
        long dcpl_id = -1;
        long dataspace_id = -1;
        long dataset_id = -1;
        long[] dims = { DIM_X, DIM_Y };
        long[] chunk_dims = { CHUNK_X, CHUNK_Y };
        long[] maxdims = { HDF5Constants.H5S_UNLIMITED, HDF5Constants.H5S_UNLIMITED };
        int[][] dset_data = new int[DIM_X][DIM_Y];

        // Initialize the dataset.
        for (int indx = 0; indx < DIM_X; indx++)
            for (int jndx = 0; jndx < DIM_Y; jndx++)
                dset_data[indx][jndx] = indx * jndx - jndx;

        // Create a new file using default properties.
        try {
            file_id = H5.H5Fcreate(FILENAME, HDF5Constants.H5F_ACC_TRUNC, HDF5Constants.H5P_DEFAULT,
                    HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create dataspace with unlimited dimensions.
        try {
            dataspace_id = H5.H5Screate_simple(RANK, dims, maxdims);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create the dataset creation property list.
        try {
            dcpl_id = H5.H5Pcreate(HDF5Constants.H5P_DATASET_CREATE);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Set the chunk size.
        try {
            if (dcpl_id >= 0)
                H5.H5Pset_chunk(dcpl_id, NDIMS, chunk_dims);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create the unlimited dataset.
        try {
            if ((file_id >= 0) && (dataspace_id >= 0) && (dcpl_id >= 0))
                dataset_id = H5.H5Dcreate(file_id, DATASETNAME, HDF5Constants.H5T_STD_I32LE, dataspace_id,
                        HDF5Constants.H5P_DEFAULT, dcpl_id, HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Write the data to the dataset.
        try {
            if (dataset_id >= 0)
                H5.H5Dwrite(dataset_id, HDF5Constants.H5T_NATIVE_INT, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
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

        try {
            if (dataspace_id >= 0)
                H5.H5Sclose(dataspace_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        try {
            if (dcpl_id >= 0)
                H5.H5Pclose(dcpl_id);
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

    private static void extendUnlimited() {
        long file_id = -1;
        long dataspace_id = -1;
        long dataset_id = -1;
        long[] dims = { DIM_X, DIM_Y };
        long[] extdims = { EDIM_X, EDIM_Y };
        long[] start = { 0, 0 };
        long[] count = new long[2];
        int[][] dset_data;
        int[][] extend_dset_data = new int[EDIM_X][EDIM_Y];

        // Open an existing file.
        try {
            file_id = H5.H5Fopen(FILENAME, HDF5Constants.H5F_ACC_RDWR, HDF5Constants.H5P_DEFAULT);
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

        // Get dataspace and allocate memory for read buffer. This is a
        // two dimensional dataset so the dynamic allocation must be done
        // in steps.
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

        // Allocate array of pointers to rows.
        dset_data = new int[(int) dims[0]][(int) dims[1]];

        // Read the data using the default properties.
        try {
            if (dataset_id >= 0)
                H5.H5Dread(dataset_id, HDF5Constants.H5T_NATIVE_INT, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
                        HDF5Constants.H5P_DEFAULT, dset_data);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Output the data to the screen.
        System.out.println("Dataset before extension:");
        for (int indx = 0; indx < DIM_X; indx++) {
            System.out.print(" [ ");
            for (int jndx = 0; jndx < DIM_Y; jndx++)
                System.out.print(dset_data[indx][jndx] + " ");
            System.out.println("]");
        }
        System.out.println();

        try {
            if (dataspace_id >= 0)
                H5.H5Sclose(dataspace_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Extend the dataset.
        try {
            if (dataset_id >= 0)
                H5.H5Dset_extent(dataset_id, extdims);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Retrieve the dataspace for the newly extended dataset.
        try {
            if (dataset_id >= 0)
                dataspace_id = H5.H5Dget_space(dataset_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Initialize data for writing to the extended dataset.
        for (int indx = 0; indx < EDIM_X; indx++)
            for (int jndx = 0; jndx < EDIM_Y; jndx++)
                extend_dset_data[indx][jndx] = jndx;

        // Select the entire dataspace.
        try {
            if (dataspace_id >= 0) {
                H5.H5Sselect_all(dataspace_id);

                // Subtract a hyperslab reflecting the original dimensions from the
                // selection. The selection now contains only the newly extended
                // portions of the dataset.
                count[0] = dims[0];
                count[1] = dims[1];
                H5.H5Sselect_hyperslab(dataspace_id, HDF5Constants.H5S_SELECT_NOTB, start, null, count, null);

                // Write the data to the selected portion of the dataset.
                if (dataset_id >= 0)
                    H5.H5Dwrite(dataset_id, HDF5Constants.H5T_NATIVE_INT, HDF5Constants.H5S_ALL, dataspace_id,
                            HDF5Constants.H5P_DEFAULT, extend_dset_data);
            }
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

    private static void readUnlimited() {
        long file_id = -1;
        long dataspace_id = -1;
        long dataset_id = -1;
        long[] dims = { DIM_X, DIM_Y };
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

        // Get dataspace and allocate memory for the read buffer as before.
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
        // Allocate array of pointers to rows.
        dset_data = new int[(int) dims[0]][(int) dims[1]];

        // Read the data using the default properties.
        try {
            if (dataset_id >= 0)
                H5.H5Dread(dataset_id, HDF5Constants.H5T_NATIVE_INT, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
                        HDF5Constants.H5P_DEFAULT, dset_data);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Output the data to the screen.
        System.out.println("Dataset after extension:");
        for (int indx = 0; indx < dims[0]; indx++) {
            System.out.print(" [ ");
            for (int jndx = 0; jndx < dims[1]; jndx++)
                System.out.print(dset_data[indx][jndx] + " ");
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
        H5Ex_D_UnlimitedAdd.writeUnlimited();
        H5Ex_D_UnlimitedAdd.extendUnlimited();
        H5Ex_D_UnlimitedAdd.readUnlimited();
    }

}
