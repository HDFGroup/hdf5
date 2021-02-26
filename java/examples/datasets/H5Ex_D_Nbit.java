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
 This example shows how to read and write data to a dataset
 using the N-Bit filter.  The program first checks if the
 N-Bit filter is available, then if it is it writes integers
 to a dataset using N-Bit, then closes the file. Next, it
 reopens the file, reads back the data, and outputs the type
 of filter and the maximum value in the dataset to the screen.
 ************************************************************/

package examples.datasets;

import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;

import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;

public class H5Ex_D_Nbit {
    private static String FILENAME = "H5Ex_D_Nbit.h5";
    private static String DATASETNAME = "DS1";
    private static final int DIM_X = 32;
    private static final int DIM_Y = 64;
    private static final int CHUNK_X = 4;
    private static final int CHUNK_Y = 8;
    private static final int RANK = 2;
    private static final int NDIMS = 2;

    // Values for the status of space allocation
    enum H5Z_filter {
        H5Z_FILTER_ERROR(HDF5Constants.H5Z_FILTER_ERROR), H5Z_FILTER_NONE(HDF5Constants.H5Z_FILTER_NONE), H5Z_FILTER_DEFLATE(
                HDF5Constants.H5Z_FILTER_DEFLATE), H5Z_FILTER_SHUFFLE(HDF5Constants.H5Z_FILTER_SHUFFLE), H5Z_FILTER_FLETCHER32(
                HDF5Constants.H5Z_FILTER_FLETCHER32), H5Z_FILTER_SZIP(HDF5Constants.H5Z_FILTER_SZIP), H5Z_FILTER_NBIT(
                HDF5Constants.H5Z_FILTER_NBIT), H5Z_FILTER_SCALEOFFSET(HDF5Constants.H5Z_FILTER_SCALEOFFSET), H5Z_FILTER_RESERVED(
                HDF5Constants.H5Z_FILTER_RESERVED), H5Z_FILTER_MAX(HDF5Constants.H5Z_FILTER_MAX);
        private static final Map<Integer, H5Z_filter> lookup = new HashMap<Integer, H5Z_filter>();

        static {
            for (H5Z_filter s : EnumSet.allOf(H5Z_filter.class))
                lookup.put(s.getCode(), s);
        }

        private int code;

        H5Z_filter(int layout_type) {
            this.code = layout_type;
        }

        public int getCode() {
            return this.code;
        }

        public static H5Z_filter get(int code) {
            return lookup.get(code);
        }
    }

    private static boolean checkNbitFilter() {
        try {
            // Check if N-Bit compression is available and can be used for both compression and decompression.
            int available = H5.H5Zfilter_avail(HDF5Constants.H5Z_FILTER_NBIT);
            if (available == 0) {
                System.out.println("N-Bit filter not available.");
                return false;
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        try {
            int filter_info = H5.H5Zget_filter_info(HDF5Constants.H5Z_FILTER_NBIT);
            if (((filter_info & HDF5Constants.H5Z_FILTER_CONFIG_ENCODE_ENABLED) == 0)
                    || ((filter_info & HDF5Constants.H5Z_FILTER_CONFIG_DECODE_ENABLED) == 0)) {
                System.out.println("N-Bit filter not available for encoding and decoding.");
                return false;
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }
        return true;
    }

    private static void writeData() throws Exception {
        long file_id = HDF5Constants.H5I_INVALID_HID;
        long filespace_id = HDF5Constants.H5I_INVALID_HID;
        long dataset_id = HDF5Constants.H5I_INVALID_HID;
        long dtype_id = HDF5Constants.H5I_INVALID_HID;
        long dcpl_id = HDF5Constants.H5I_INVALID_HID;
        long[] dims = { DIM_X, DIM_Y };
        long[] chunk_dims = { CHUNK_X, CHUNK_Y };
        int[][] dset_data = new int[DIM_X][DIM_Y];

        // Initialize data.
        for (int indx = 0; indx < DIM_X; indx++)
            for (int jndx = 0; jndx < DIM_Y; jndx++)
                dset_data[indx][jndx] = indx * jndx - jndx;

        try {
            // Create a new file using the default properties.
            file_id = H5.H5Fcreate(FILENAME, HDF5Constants.H5F_ACC_TRUNC, HDF5Constants.H5P_DEFAULT,
                    HDF5Constants.H5P_DEFAULT);

            // Create dataspace. Setting maximum size to NULL sets the maximum
            // size to be the current size.
            filespace_id = H5.H5Screate_simple(RANK, dims, null);

            // Create the datatype to use with the N-Bit filter. It has an uncompressed size of 32 bits,
            // but will have a size of 16 bits after being packed by the N-Bit filter.
            dtype_id = H5.H5Tcopy(HDF5Constants.H5T_STD_I32LE);
            H5.H5Tset_precision(dtype_id, 16);
            H5.H5Tset_offset(dtype_id, 5);

            // Create the dataset creation property list, add the N-Bit filter and set the chunk size.
            dcpl_id = H5.H5Pcreate(HDF5Constants.H5P_DATASET_CREATE);
            H5.H5Pset_nbit(dcpl_id);
            H5.H5Pset_chunk(dcpl_id, NDIMS, chunk_dims);

            // Create the dataset.
            dataset_id = H5.H5Dcreate(file_id, DATASETNAME, dtype_id, filespace_id, HDF5Constants.H5P_DEFAULT, dcpl_id,
                    HDF5Constants.H5P_DEFAULT);

            // Write the data to the dataset.
            H5.H5Dwrite(dataset_id, HDF5Constants.H5T_NATIVE_INT, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
                    HDF5Constants.H5P_DEFAULT, dset_data);
        }
        catch (Exception e) {
            e.printStackTrace();
        }
        finally {
            // Close and release resources.
            if (dcpl_id >= 0)
                H5.H5Pclose(dcpl_id);
            if (dtype_id >= 0)
                H5.H5Tclose(dtype_id);
            if (dataset_id >= 0)
                H5.H5Dclose(dataset_id);
            if (filespace_id >= 0)
                H5.H5Sclose(filespace_id);
            if (file_id >= 0)
                H5.H5Fclose(file_id);
        }
    }

    private static void readData() throws Exception {
        long file_id = HDF5Constants.H5I_INVALID_HID;
        long dataset_id = HDF5Constants.H5I_INVALID_HID;
        long dcpl_id = HDF5Constants.H5I_INVALID_HID;
        int[][] dset_data = new int[DIM_X][DIM_Y];

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

        // Retrieve the dataset creation property list.
        try {
            if (dataset_id >= 0)
                dcpl_id = H5.H5Dget_create_plist(dataset_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Retrieve and print the filter type. Here we only retrieve the
        // first filter because we know that we only added one filter.
        try {
            if (dcpl_id >= 0) {
                // Java lib requires a valid filter_name object and cd_values
                int[] flags = { 0 };
                long[] cd_nelmts = { 1 };
                int[] cd_values = { 0 };
                String[] filter_name = { "" };
                int[] filter_config = { 0 };
                int filter_type = -1;
                filter_type = H5
                        .H5Pget_filter(dcpl_id, 0, flags, cd_nelmts, cd_values, 120, filter_name, filter_config);
                System.out.print("Filter type is: ");
                switch (H5Z_filter.get(filter_type)) {
                case H5Z_FILTER_DEFLATE:
                    System.out.println("H5Z_FILTER_DEFLATE");
                    break;
                case H5Z_FILTER_SHUFFLE:
                    System.out.println("H5Z_FILTER_SHUFFLE");
                    break;
                case H5Z_FILTER_FLETCHER32:
                    System.out.println("H5Z_FILTER_FLETCHER32");
                    break;
                case H5Z_FILTER_SZIP:
                    System.out.println("H5Z_FILTER_SZIP");
                    break;
                case H5Z_FILTER_NBIT:
                    System.out.println("H5Z_FILTER_NBIT");
                    break;
                case H5Z_FILTER_SCALEOFFSET:
                    System.out.println("H5Z_FILTER_SCALEOFFSET");
                    break;
                default:
                    System.out.println("H5Z_FILTER_ERROR");
                }
                System.out.println();
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Read the data using the default properties.
        try {
            if (dataset_id >= 0) {
                int status = H5.H5Dread(dataset_id, HDF5Constants.H5T_NATIVE_INT, HDF5Constants.H5S_ALL,
                        HDF5Constants.H5S_ALL, HDF5Constants.H5P_DEFAULT, dset_data);
                // Check if the read was successful.
                if (status < 0)
                    System.out.print("Dataset read failed!");
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Find the maximum value in the dataset, to verify that it was read
        // correctly.
        int max = dset_data[0][0];
        for (int indx = 0; indx < DIM_X; indx++) {
            for (int jndx = 0; jndx < DIM_Y; jndx++)
                if (max < dset_data[indx][jndx])
                    max = dset_data[indx][jndx];
        }
        // Print the maximum value.
        System.out.println("Maximum value in " + DATASETNAME + " is: " + max);

        // End access to the dataset and release resources used by it.
        try {
            if (dcpl_id >= 0)
                H5.H5Pclose(dcpl_id);
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
        /*
         * Check if N-Bit compression is available and can be used for both compression and decompression. Normally we
         * do not perform error checking in these examples for the sake of clarity, but in this case we will make an
         * exception because this filter is an optional part of the hdf5 library.
         */
        try {
            if (H5Ex_D_Nbit.checkNbitFilter()) {
                H5Ex_D_Nbit.writeData();
                H5Ex_D_Nbit.readData();
            }
        }
        catch (Exception ex) {
            ex.printStackTrace();
        }
    }
}
