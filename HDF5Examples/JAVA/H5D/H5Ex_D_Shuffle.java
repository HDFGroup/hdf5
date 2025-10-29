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
  This example shows how to read and write data to a dataset
  using the shuffle filter with gzip compression.  The
  program first checks if the shuffle and gzip filters are
  available, then if they are it writes integers to a
  dataset using shuffle+gzip, then closes the file.  Next,
  it reopens the file, reads back the data, and outputs the
  types of filters and the maximum value in the dataset to
  the screen.
 ************************************************************/

import static org.hdfgroup.javahdf5.hdf5_h.*;

import java.lang.foreign.Arena;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.ValueLayout;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;

public class H5Ex_D_Shuffle {
    private static String FILENAME    = "H5Ex_D_Shuffle.h5";
    private static String DATASETNAME = "DS1";
    private static final int DIM_X    = 32;
    private static final int DIM_Y    = 64;
    private static final int CHUNK_X  = 4;
    private static final int CHUNK_Y  = 8;
    private static final int RANK     = 2;
    private static final int NDIMS    = 2;

    // Values for the status of space allocation
    enum H5Z_filter {
        H5Z_FILTER_ERROR(H5Z_FILTER_ERROR()),
        H5Z_FILTER_NONE(H5Z_FILTER_NONE()),
        H5Z_FILTER_DEFLATE(H5Z_FILTER_DEFLATE()),
        H5Z_FILTER_SHUFFLE(H5Z_FILTER_SHUFFLE()),
        H5Z_FILTER_FLETCHER32(H5Z_FILTER_FLETCHER32()),
        H5Z_FILTER_SZIP(H5Z_FILTER_SZIP()),
        H5Z_FILTER_NBIT(H5Z_FILTER_NBIT()),
        H5Z_FILTER_SCALEOFFSET(H5Z_FILTER_SCALEOFFSET()),
        H5Z_FILTER_RESERVED(H5Z_FILTER_RESERVED()),
        H5Z_FILTER_MAX(H5Z_FILTER_MAX());

        private static final Map<Integer, H5Z_filter> lookup = new HashMap<Integer, H5Z_filter>();

        static
        {
            for (H5Z_filter s : EnumSet.allOf(H5Z_filter.class))
                lookup.put(s.getCode(), s);
        }

        private int code;

        H5Z_filter(int layout_type) { this.code = layout_type; }

        public int getCode() { return this.code; }

        public static H5Z_filter get(int code) { return lookup.get(code); }
    }

    private static boolean checkGzipFilter()
    {
        try {
            int available = H5Zfilter_avail(H5Z_FILTER_DEFLATE());
            if (available == 0) {
                System.out.println("gzip filter not available.");
                return false;
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        try {
            try (Arena arena = Arena.ofConfined()) {
                MemorySegment filterInfoSeg = arena.allocate(ValueLayout.JAVA_INT);
                H5Zget_filter_info(H5Z_FILTER_DEFLATE(), filterInfoSeg);
                int filter_info = filterInfoSeg.get(ValueLayout.JAVA_INT, 0);
                if (((filter_info & H5Z_FILTER_CONFIG_ENCODE_ENABLED()) == 0) ||
                    ((filter_info & H5Z_FILTER_CONFIG_DECODE_ENABLED()) == 0)) {
                    System.out.println("gzip filter not available for encoding and decoding.");
                    return false;
                }
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }
        return true;
    }

    private static boolean checkShuffleFilter()
    {
        try {
            int available = H5Zfilter_avail(H5Z_FILTER_SHUFFLE());
            if (available == 0) {
                System.out.println("Shuffle filter not available.");
                return false;
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        try {
            try (Arena arena = Arena.ofConfined()) {
                MemorySegment filterInfoSeg = arena.allocate(ValueLayout.JAVA_INT);
                H5Zget_filter_info(H5Z_FILTER_SHUFFLE(), filterInfoSeg);
                int filter_info = filterInfoSeg.get(ValueLayout.JAVA_INT, 0);
                if (((filter_info & H5Z_FILTER_CONFIG_ENCODE_ENABLED()) == 0) ||
                    ((filter_info & H5Z_FILTER_CONFIG_DECODE_ENABLED()) == 0)) {
                    System.out.println("Shuffle filter not available for encoding and decoding.");
                    return false;
                }
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }
        return true;
    }

    private static void writeShuffle(Arena arena)
    {
        long file_id      = H5I_INVALID_HID();
        long filespace_id = H5I_INVALID_HID();
        long dataset_id   = H5I_INVALID_HID();
        long dcpl_id      = H5I_INVALID_HID();
        long[] dims       = {DIM_X, DIM_Y};
        long[] chunk_dims = {CHUNK_X, CHUNK_Y};
        int[][] dset_data = new int[DIM_X][DIM_Y];

        // Initialize data.
        for (int indx = 0; indx < DIM_X; indx++)
            for (int jndx = 0; jndx < DIM_Y; jndx++)
                dset_data[indx][jndx] = indx * jndx - jndx;

        // Create a new file using default properties.
        try {
            file_id = H5Fcreate(arena.allocateFrom(FILENAME), H5F_ACC_TRUNC(), H5P_DEFAULT(), H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create dataspace. Setting maximum size to NULL sets the maximum
        // size to be the current size.
        try {
            filespace_id =
                H5Screate_simple(RANK, arena.allocateFrom(ValueLayout.JAVA_LONG, dims), MemorySegment.NULL);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create the dataset creation property list, add the shuffle
        // filter and the gzip compression filter.
        // The order in which the filters are added here is significant -
        // we will see much greater results when the shuffle is applied
        // first. The order in which the filters are added to the property
        // list is the order in which they will be invoked when writing
        // data.
        try {
            dcpl_id = H5Pcreate(H5P_CLS_DATASET_CREATE_ID_g());
            if (dcpl_id >= 0) {
                H5Pset_shuffle(dcpl_id);
                H5Pset_deflate(dcpl_id, 9);
                // Set the chunk size.
                H5Pset_chunk(dcpl_id, NDIMS, arena.allocateFrom(ValueLayout.JAVA_LONG, chunk_dims));
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create the dataset.
        try {
            if ((file_id >= 0) && (filespace_id >= 0) && (dcpl_id >= 0))
                dataset_id = H5Dcreate2(file_id, arena.allocateFrom(DATASETNAME), H5T_STD_I32LE_g(),
                                        filespace_id, H5P_DEFAULT(), dcpl_id, H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Write the data to the dataset.
        try {
            if (dataset_id >= 0) {
                // Flatten the 2D array to 1D for MemorySegment
                int[] flatData = new int[DIM_X * DIM_Y];
                for (int i = 0; i < DIM_X; i++) {
                    for (int j = 0; j < DIM_Y; j++) {
                        flatData[i * DIM_Y + j] = dset_data[i][j];
                    }
                }
                MemorySegment dataSeg = arena.allocateFrom(ValueLayout.JAVA_INT, flatData);
                H5Dwrite(dataset_id, H5T_NATIVE_INT_g(), H5S_ALL(), H5S_ALL(), H5P_DEFAULT(), dataSeg);
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // End access to the dataset and release resources used by it.
        try {
            if (dcpl_id >= 0)
                H5Pclose(dcpl_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        try {
            if (dataset_id >= 0)
                H5Dclose(dataset_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        try {
            if (filespace_id >= 0)
                H5Sclose(filespace_id);
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

    private static void readShuffle(Arena arena)
    {
        long file_id      = H5I_INVALID_HID();
        long dataset_id   = H5I_INVALID_HID();
        long dcpl_id      = H5I_INVALID_HID();
        int[][] dset_data = new int[DIM_X][DIM_Y];

        // Open an existing file.
        try {
            file_id = H5Fopen(arena.allocateFrom(FILENAME), H5F_ACC_RDONLY(), H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Open an existing dataset.
        try {
            if (file_id >= 0)
                dataset_id = H5Dopen2(file_id, arena.allocateFrom(DATASETNAME), H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Retrieve the dataset creation property list.
        try {
            if (dataset_id >= 0)
                dcpl_id = H5Dget_create_plist(dataset_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Retrieve the number of filters, and retrieve and print the
        // type of each.
        try {
            if (dcpl_id >= 0) {
                int nfilters = H5Pget_nfilters(dcpl_id);
                for (int indx = 0; indx < nfilters; indx++) {
                    // FFM requires MemorySegment parameters
                    MemorySegment flagsSeg   = arena.allocate(ValueLayout.JAVA_INT);
                    MemorySegment cdNeltsSeg = arena.allocate(ValueLayout.JAVA_LONG);
                    cdNeltsSeg.set(ValueLayout.JAVA_LONG, 0, 10L);
                    MemorySegment cdValuesSeg     = arena.allocate(ValueLayout.JAVA_INT, 10);
                    MemorySegment nameSegment     = arena.allocate(256);
                    MemorySegment filterConfigSeg = arena.allocate(ValueLayout.JAVA_INT);
                    int filter_type = H5Pget_filter2(dcpl_id, indx, flagsSeg, cdNeltsSeg, cdValuesSeg, 256,
                                                     nameSegment, filterConfigSeg);
                    System.out.print("Filter " + indx + ": Type is: ");
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
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Read the data using the default properties.
        try {
            if (dataset_id >= 0) {
                MemorySegment dataSeg = arena.allocate(ValueLayout.JAVA_INT, DIM_X * DIM_Y);
                H5Dread(dataset_id, H5T_NATIVE_INT_g(), H5S_ALL(), H5S_ALL(), H5P_DEFAULT(), dataSeg);
                // Unflatten the 1D MemorySegment to 2D array
                for (int i = 0; i < DIM_X; i++) {
                    for (int j = 0; j < DIM_Y; j++) {
                        dset_data[i][j] = dataSeg.getAtIndex(ValueLayout.JAVA_INT, i * DIM_Y + j);
                    }
                }
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
                H5Pclose(dcpl_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

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

    public static void main(String[] args)
    {
        try (Arena arena = Arena.ofConfined()) {
            if (H5Ex_D_Shuffle.checkGzipFilter() && H5Ex_D_Shuffle.checkShuffleFilter()) {
                H5Ex_D_Shuffle.writeShuffle(arena);
                H5Ex_D_Shuffle.readShuffle(arena);
            }
        }
    }
}
