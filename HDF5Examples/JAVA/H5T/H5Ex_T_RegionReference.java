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
  This example shows how to read and write object references
  to a dataset.  The program first creates objects in the
  file and writes references to those objects to a dataset
  with a dataspace of DIM0, then closes the file.  Next, it
  reopens the file, dereferences the references, and outputs
  the names of their targets to the screen.
 ************************************************************/

import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;

import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;

public class H5Ex_T_RegionReference {
    private static String FILENAME     = "H5Ex_T_RegionReference.h5";
    private static String DATASETNAME  = "DS1";
    private static String DATASETNAME2 = "DS2";
    private static String GROUPNAME    = "G1";
    private static final int DIM0      = 2;
    private static final int DS2DIM0   = 3;
    private static final int DS2DIM1   = 16;
    private static final int RANK      = 1;

    private static void writeRegRef()
    {
        long file_id      = HDF5Constants.H5I_INVALID_HID;
        long dataspace_id = HDF5Constants.H5I_INVALID_HID;
        long filespace_id = HDF5Constants.H5I_INVALID_HID;
        long group_id     = HDF5Constants.H5I_INVALID_HID;
        long dataset_id   = HDF5Constants.H5I_INVALID_HID;
        long[] dims       = {DIM0};
        long[] dims2      = {DS2DIM0, DS2DIM1};
        // data buffer for writing region reference
        byte[][] dset_data = new byte[DIM0][HDF5Constants.H5R_REF_BUF_SIZE];
        // data buffer for writing dataset
        byte[][] write_data     = new byte[DS2DIM0][DS2DIM1];
        StringBuffer[] str_data = {new StringBuffer("The quick brown"), new StringBuffer("fox jumps over "),
                                   new StringBuffer("the 5 lazy dogs")};

        // Create a new file using default properties.
        try {
            file_id = H5.H5Fcreate(FILENAME, HDF5Constants.H5F_ACC_TRUNC, HDF5Constants.H5P_DEFAULT,
                                   HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create dataset with character data.
        try {
            dataspace_id = H5.H5Screate_simple(2, dims2, null);
            if ((file_id >= 0) && (dataspace_id >= 0)) {
                dataset_id = H5.H5Dcreate(file_id, DATASETNAME2, HDF5Constants.H5T_STD_I8LE, dataspace_id,
                                          HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT,
                                          HDF5Constants.H5P_DEFAULT);
                for (int indx = 0; indx < DS2DIM0; indx++) {
                    for (int jndx = 0; jndx < DS2DIM1; jndx++) {
                        if (jndx < str_data[indx].length())
                            write_data[indx][jndx] = (byte)str_data[indx].charAt(jndx);
                        else
                            write_data[indx][jndx] = 0;
                    }
                }
                H5.H5Dwrite(dataset_id, HDF5Constants.H5T_NATIVE_CHAR, HDF5Constants.H5S_ALL,
                            HDF5Constants.H5S_ALL, HDF5Constants.H5P_DEFAULT, write_data);
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create reference to a list of elements in dset2.
        try {
            long[][] coords = {{0, 1}, {2, 11}, {1, 0}, {2, 4}};

            H5.H5Sselect_elements(dataspace_id, HDF5Constants.H5S_SELECT_SET, 4, coords);
            if (file_id >= 0)
                dset_data[0] =
                    H5.H5Rcreate_region(file_id, DATASETNAME2, dataspace_id, HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception ex) {
            ex.printStackTrace();
        }

        // Create reference to a hyperslab in dset2.
        try {
            long[] start  = {0, 0};  // Starting location of hyperslab
            long[] stride = {2, 11}; // Stride of hyperslab
            long[] count  = {2, 2};  // Element count of hyperslab
            long[] block  = {1, 3};  // Block size of hyperslab

            H5.H5Sselect_hyperslab(dataspace_id, HDF5Constants.H5S_SELECT_SET, start, stride, count, block);
            if (file_id >= 0)
                dset_data[1] =
                    H5.H5Rcreate_region(file_id, DATASETNAME2, dataspace_id, HDF5Constants.H5P_DEFAULT);
            ;
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        try {
            H5.H5Sclose(dataspace_id);
        }
        catch (Exception e) {
        }

        // Create the dataset and write the region references to it.
        try {
            dataspace_id = H5.H5Screate_simple(1, dims, null);
            if ((file_id >= 0) && (dataspace_id >= 0)) {
                dataset_id = H5.H5Dcreate(file_id, DATASETNAME, HDF5Constants.H5T_STD_REF, dataspace_id,
                                          HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT,
                                          HDF5Constants.H5P_DEFAULT);
                H5.H5Dwrite(dataset_id, HDF5Constants.H5T_STD_REF, HDF5Constants.H5S_ALL,
                            HDF5Constants.H5S_ALL, HDF5Constants.H5P_DEFAULT, dset_data);
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        try {
            H5.H5Rdestroy(dset_data[0]);
        }
        catch (Exception ex) {
        }

        try {
            H5.H5Rdestroy(dset_data[1]);
        }
        catch (Exception ex) {
        }

        // End access to the dataset and release resources used by it.
        try {
            H5.H5Sclose(dataspace_id);
        }
        catch (Exception e) {
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

    private static void readRegRef()
    {
        long file_id       = HDF5Constants.H5I_INVALID_HID;
        long dataset_id    = HDF5Constants.H5I_INVALID_HID;
        long dataspace_id  = HDF5Constants.H5I_INVALID_HID;
        int object_type    = -1;
        long object_id     = HDF5Constants.H5I_INVALID_HID;
        long region_id     = HDF5Constants.H5I_INVALID_HID;
        long[] dims        = {DIM0};
        byte[][] dset_data = new byte[DIM0][HDF5Constants.H5R_REF_BUF_SIZE];
        StringBuffer str_data;

        // Open an existing file.
        try {
            file_id = H5.H5Fopen(FILENAME, HDF5Constants.H5F_ACC_RDONLY, HDF5Constants.H5P_DEFAULT);

            // Open an existing dataset.
            try {
                dataset_id = H5.H5Dopen(file_id, DATASETNAME, HDF5Constants.H5P_DEFAULT);

                try {
                    // Get dataspace and allocate memory for read buffer.
                    dataspace_id = H5.H5Dget_space(dataset_id);
                    H5.H5Sget_simple_extent_dims(dataspace_id, dims, null);

                    // Read data.
                    H5.H5Dread(dataset_id, HDF5Constants.H5T_STD_REF, HDF5Constants.H5S_ALL,
                               HDF5Constants.H5S_ALL, HDF5Constants.H5P_DEFAULT, dset_data);

                    // Output the data to the screen.
                    for (int indx = 0; indx < dims[0]; indx++) {
                        System.out.println(DATASETNAME + "[" + indx + "]:");
                        System.out.print("  ->");
                        // Open the referenced object.
                        try {
                            object_id = H5.H5Ropen_object(dset_data[indx], HDF5Constants.H5P_DEFAULT,
                                                          HDF5Constants.H5P_DEFAULT);
                            try {
                                String obj_name = H5.H5Iget_name(object_id);

                                region_id = H5.H5Ropen_region(dset_data[indx], HDF5Constants.H5P_DEFAULT,
                                                              HDF5Constants.H5P_DEFAULT);
                                if ((object_id >= 0) && (region_id >= 0)) {
                                    try {
                                        long reg_npoints = H5.H5Sget_select_npoints(region_id);
                                        long[] dims2     = new long[1];
                                        dims2[0]         = (int)reg_npoints;
                                        dataspace_id     = H5.H5Screate_simple(1, dims2, null);

                                        // Read data.
                                        byte[] refbuf = new byte[(int)reg_npoints + 1];
                                        H5.H5Dread(object_id, HDF5Constants.H5T_NATIVE_CHAR, dataspace_id,
                                                   region_id, HDF5Constants.H5P_DEFAULT, refbuf);
                                        refbuf[(int)reg_npoints] = 0;
                                        str_data = new StringBuffer(new String(refbuf).trim());

                                        System.out.println(" " + obj_name + ": " + str_data);
                                    }
                                    catch (Throwable err2) {
                                        err2.printStackTrace();
                                    }
                                }
                            }
                            catch (Throwable err1) {
                                err1.printStackTrace();
                            }
                            finally {
                                try {
                                    H5.H5Sclose(region_id);
                                }
                                catch (Exception ex) {
                                }
                            }
                        }
                        catch (Throwable err0) {
                            err0.printStackTrace();
                        }
                        finally {
                            try {
                                H5.H5Dclose(object_id);
                            }
                            catch (Exception ex) {
                            }
                        }
                        try {
                            H5.H5Rdestroy(dset_data[indx]);
                        }
                        catch (Exception e4) {
                        }
                    } // end for
                }
                catch (Exception e4) {
                    e4.printStackTrace();
                }
                finally {
                    try {
                        H5.H5Sclose(dataspace_id);
                        for (int indx = 0; indx < dims[0]; indx++)
                            H5.H5Rdestroy(dset_data[indx]);
                    }
                    catch (Exception e4) {
                    }
                }
            }
            catch (Exception e3) {
                e3.printStackTrace();
            }
            finally {
                try {
                    H5.H5Dclose(dataset_id);
                }
                catch (Exception e3) {
                }
            }
        }
        catch (Exception e2) {
            e2.printStackTrace();
        }
        finally {
            try {
                H5.H5Fclose(file_id);
            }
            catch (Exception e2) {
            }
        }
    }

    public static void main(String[] args)
    {
        H5Ex_T_RegionReference.writeRegRef();
        H5Ex_T_RegionReference.readRegRef();
    }
}
