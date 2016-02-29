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
      This example shows how to read and write data to a dataset
      using a data transform expression.  The program first
      writes integers to a dataset using the transform
      expression TRANSFORM, then closes the file.  Next, it
      reopens the file, reads back the data without a transform,
      and outputs the data to the screen.  Finally it reads the
      data using the transform expression RTRANSFORM and outputs
      the results to the screen.
 ************************************************************/
package examples.datasets;

import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;

public class H5Ex_D_Transform {

    private static String FILE = "H5Ex_D_Transform.h5";
    private static String DATASET = "DS1";
    private static final int DIM_X = 4;
    private static final int DIM_Y = 7;
    private static String TRANSFORM = "x+1";
    private static String RTRANSFORM = "x-1";

    private static void writeData() {
        long file_id = -1;
        long filespace_id = -1;
        long dataset_id = -1;
        long dxpl_id = -1;

        long[] dims = { DIM_X, DIM_Y };
        int[][] dset_data = new int[DIM_X][DIM_Y];

        // Initialize data.
        for (int i = 0; i < DIM_X; i++)
            for (int j = 0; j < DIM_Y; j++)
                dset_data[i][j] = i * j - j;

        // Output the data to the screen.
        System.out.println("Original Data:");
        for (int i = 0; i < DIM_X; i++) {
            System.out.print(" [");
            for (int j = 0; j < DIM_Y; j++)
                System.out.print(" " + dset_data[i][j] + " ");
            System.out.println("]");
        }

        // Create a new file using the default properties.
        try {
            file_id = H5.H5Fcreate(FILE, HDF5Constants.H5F_ACC_TRUNC, HDF5Constants.H5P_DEFAULT,
                    HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create dataspace. Setting maximum size to NULL sets the maximum
        // size to be the current size.
        try {
            filespace_id = H5.H5Screate_simple(2, dims, null);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create the dataset transfer property list and define the transform expression.
        try {
            dxpl_id = H5.H5Pcreate(HDF5Constants.H5P_DATASET_XFER);
            if (dxpl_id >= 0)
                H5.H5Pset_data_transform(dxpl_id, TRANSFORM);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create the dataset using the default properties. Unfortunately we must save as
        // a native type or the transform operation will fail.
        try {
            if ((file_id >= 0) && (filespace_id >= 0))
                dataset_id = H5.H5Dcreate(file_id, DATASET, HDF5Constants.H5T_NATIVE_INT, filespace_id,
                        HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Write the data to the dataset using the dataset transfer property list.
        try {
            if ((dataset_id >= 0) && (dxpl_id >= 0))
                H5.H5Dwrite(dataset_id, HDF5Constants.H5T_NATIVE_INT, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
                        dxpl_id, dset_data);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // End access to the dataset and release resources used by it.
        try {
            if (dxpl_id >= 0)
                H5.H5Pclose(dxpl_id);
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

        try {
            if (filespace_id >= 0)
                H5.H5Sclose(filespace_id);
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

    private static void readData() {

        long file_id = -1;
        long dataset_id = -1;
        long dxpl_id = -1;
        int[][] dset_data = new int[DIM_X][DIM_Y];

        // Open an existing file using the default properties.
        try {
            file_id = H5.H5Fopen(FILE, HDF5Constants.H5F_ACC_RDONLY, HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Open an existing dataset using the default properties.
        try {
            if (file_id >= 0)
                dataset_id = H5.H5Dopen(file_id, DATASET, HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

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
        System.out.println("Data as written with transform '" + TRANSFORM + "'");
        for (int i = 0; i < DIM_X; i++) {
            System.out.print(" [");
            for (int j = 0; j < DIM_Y; j++)
                System.out.print(" " + dset_data[i][j] + " ");
            System.out.println("]");
        }

        // Create the dataset transfer property list and define the transform expression.
        try {
            dxpl_id = H5.H5Pcreate(HDF5Constants.H5P_DATASET_XFER);
            if (dxpl_id >= 0)
                H5.H5Pset_data_transform(dxpl_id, RTRANSFORM);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Read the data using the dataset transfer property list.
        try {
            if ((dataset_id >= 0) && (dxpl_id >= 0))
                H5.H5Dread(dataset_id, HDF5Constants.H5T_NATIVE_INT, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
                        dxpl_id, dset_data);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Output the data to the screen.

        System.out.println("Data as written with transform  '" + TRANSFORM + "' and read with transform  '"
                + RTRANSFORM + "'");
        for (int i = 0; i < DIM_X; i++) {
            System.out.print(" [");
            for (int j = 0; j < DIM_Y; j++)
                System.out.print(" " + dset_data[i][j] + " ");
            System.out.println("]");
        }

        // Close and release resources.
        try {
            if (dxpl_id >= 0)
                H5.H5Pclose(dxpl_id);
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
        try {
            if (file_id >= 0)
                H5.H5Fclose(file_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static void main(String[] args) {
        H5Ex_D_Transform.writeData();
        H5Ex_D_Transform.readData();
    }

}
