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
  This example shows how to read and write object references
  to a dataset.  The program first creates objects in the
  file and writes references to those objects to a dataset
  with a dataspace of DIM0, then closes the file.  Next, it
  reopens the file, dereferences the references, and outputs
  the names of their targets to the screen.
 ************************************************************/

import static org.hdfgroup.javahdf5.hdf5_h.*;

import java.lang.foreign.Arena;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.ValueLayout;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;

public class H5Ex_T_RegionReferenceAttribute {
    private static String FILENAME      = "H5Ex_T_RegionReferenceAttribute.h5";
    private static String DATASETNAME   = "DS1";
    private static String ATTRIBUTENAME = "A1";
    private static String DATASETNAME2  = "DS2";
    private static String GROUPNAME     = "G1";
    private static final int DIM0       = 2;
    private static final int DS2DIM0    = 3;
    private static final int DS2DIM1    = 16;
    private static final int RANK       = 1;

    private static void writeRegRef(Arena arena)
    {
        long file_id      = H5I_INVALID_HID();
        long dataspace_id = H5I_INVALID_HID();
        long filespace_id = H5I_INVALID_HID();
        long group_id     = H5I_INVALID_HID();
        long dataset_id   = H5I_INVALID_HID();
        long attribute_id = H5I_INVALID_HID();
        long[] dims       = {DIM0};
        long[] dims2      = {DS2DIM0, DS2DIM1};
        // data buffer for writing region reference
        // Allocate MemorySegments for references
        MemorySegment[] refs = new MemorySegment[DIM0];
        for (int i = 0; i < DIM0; i++) {
            refs[i] = arena.allocate(H5R_REF_BUF_SIZE());
        }
        // data buffer for writing dataset
        byte[][] write_data     = new byte[DS2DIM0][DS2DIM1];
        StringBuffer[] str_data = {new StringBuffer("The quick brown"), new StringBuffer("fox jumps over "),
                                   new StringBuffer("the 5 lazy dogs")};

        // Create a new file using default properties.
        try {
            file_id = H5Fcreate(arena.allocateFrom(FILENAME), H5F_ACC_TRUNC(), H5P_DEFAULT(), H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create dataset with character data.
        try {
            dataspace_id =
                H5Screate_simple(2, arena.allocateFrom(ValueLayout.JAVA_LONG, dims2), MemorySegment.NULL);
            if ((file_id >= 0) && (dataspace_id >= 0)) {
                dataset_id = H5Dcreate2(file_id, arena.allocateFrom(DATASETNAME2), H5T_STD_I8LE_g(),
                                        dataspace_id, H5P_DEFAULT(), H5P_DEFAULT(), H5P_DEFAULT());
                for (int indx = 0; indx < DS2DIM0; indx++) {
                    for (int jndx = 0; jndx < DS2DIM1; jndx++) {
                        if (jndx < str_data[indx].length())
                            write_data[indx][jndx] = (byte)str_data[indx].charAt(jndx);
                        else
                            write_data[indx][jndx] = 0;
                    }
                }
                // Flatten 2D byte array to 1D for MemorySegment
                byte[] flatData = new byte[DS2DIM0 * DS2DIM1];
                for (int i = 0; i < DS2DIM0; i++) {
                    for (int j = 0; j < DS2DIM1; j++) {
                        flatData[i * DS2DIM1 + j] = write_data[i][j];
                    }
                }
                MemorySegment writeSeg = arena.allocateFrom(ValueLayout.JAVA_BYTE, flatData);
                H5Dwrite(dataset_id, H5T_STD_I8LE_g(), H5S_ALL(), H5S_ALL(), H5P_DEFAULT(), writeSeg);
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create reference to a list of elements in dset2.
        try {
            long[][] coords = {{0, 1}, {2, 11}, {1, 0}, {2, 4}};
            // Flatten coords for MemorySegment
            long[] flatCoords = new long[4 * 2];
            for (int i = 0; i < 4; i++) {
                flatCoords[i * 2]     = coords[i][0];
                flatCoords[i * 2 + 1] = coords[i][1];
            }
            MemorySegment coordsSeg = arena.allocateFrom(ValueLayout.JAVA_LONG, flatCoords);
            H5Sselect_elements(dataspace_id, H5S_SELECT_SET(), 4, coordsSeg);
            if (file_id >= 0)
                H5Rcreate_region(file_id, arena.allocateFrom(DATASETNAME2), dataspace_id, H5P_DEFAULT(),
                                 refs[0]);
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
            // Convert arrays to MemorySegments
            MemorySegment startSeg  = arena.allocateFrom(ValueLayout.JAVA_LONG, start);
            MemorySegment strideSeg = arena.allocateFrom(ValueLayout.JAVA_LONG, stride);
            MemorySegment countSeg  = arena.allocateFrom(ValueLayout.JAVA_LONG, count);
            MemorySegment blockSeg  = arena.allocateFrom(ValueLayout.JAVA_LONG, block);
            H5Sselect_hyperslab(dataspace_id, H5S_SELECT_SET(), startSeg, strideSeg, countSeg, blockSeg);
            if (file_id >= 0)
                H5Rcreate_region(file_id, arena.allocateFrom(DATASETNAME2), dataspace_id, H5P_DEFAULT(),
                                 refs[1]);
            ;
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        try {
            H5Sclose(dataspace_id);
        }
        catch (Exception e) {
        }

        // Create dataset with a null dataspace to serve as the parent for the attribute.
        try {
            dataspace_id = H5Screate(H5S_NULL());
            dataset_id = H5Dcreate2(file_id, arena.allocateFrom(DATASETNAME), H5T_STD_I32LE_g(), dataspace_id,
                                    H5P_DEFAULT(), H5P_DEFAULT(), H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        try {
            H5Sclose(dataspace_id);
        }
        catch (Exception e) {
        }

        // Create the attribute and write the region references to it.
        try {
            dataspace_id =
                H5Screate_simple(1, arena.allocateFrom(ValueLayout.JAVA_LONG, dims), MemorySegment.NULL);
            if ((file_id >= 0) && (attribute_id >= 0)) {
                attribute_id = H5Acreate2(file_id, arena.allocateFrom(ATTRIBUTENAME), H5T_STD_REF_g(),
                                          dataspace_id, H5P_DEFAULT(), H5P_DEFAULT());
                // Pack references into contiguous MemorySegment
                int refSize           = H5R_REF_BUF_SIZE();
                MemorySegment refData = arena.allocate(refSize * DIM0);
                for (int i = 0; i < DIM0; i++) {
                    MemorySegment.copy(refs[i], 0, refData, i * refSize, refSize);
                }
                H5Awrite(attribute_id, H5T_STD_REF_g(), refData);
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        try {
            H5Rdestroy(refs[0]);
        }
        catch (Exception ex) {
        }

        try {
            H5Rdestroy(refs[1]);
        }
        catch (Exception ex) {
        }

        // End access to theattribute, dataset and release resources used by it.
        try {
            H5Sclose(dataspace_id);
        }
        catch (Exception e) {
        }

        try {
            if (attribute_id >= 0)
                H5Aclose(attribute_id);
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

    private static void readRegRef(Arena arena)
    {
        long file_id      = H5I_INVALID_HID();
        long dataset_id   = H5I_INVALID_HID();
        long dataspace_id = H5I_INVALID_HID();
        long attribute_id = H5I_INVALID_HID();
        int object_type   = -1;
        long object_id    = H5I_INVALID_HID();
        long region_id    = H5I_INVALID_HID();
        long[] dims       = {DIM0};
        // Allocate MemorySegments for references
        MemorySegment[] refs = new MemorySegment[DIM0];
        for (int i = 0; i < DIM0; i++) {
            refs[i] = arena.allocate(H5R_REF_BUF_SIZE());
        }
        StringBuffer str_data;

        // Open an existing file.
        try {
            file_id = H5Fopen(arena.allocateFrom(FILENAME), H5F_ACC_RDONLY(), H5P_DEFAULT());

            // Open an existing attribute.
            try {
                attribute_id = H5Aopen(file_id, arena.allocateFrom(ATTRIBUTENAME), H5P_DEFAULT());

                try {
                    // Get dataspace and allocate memory for read buffer.
                    dataspace_id          = H5Aget_space(attribute_id);
                    MemorySegment dimsSeg = arena.allocateFrom(ValueLayout.JAVA_LONG, dims);
                    H5Sget_simple_extent_dims(attribute_id, dimsSeg, MemorySegment.NULL);
                    // Read back the dimensions
                    for (int i = 0; i < dims.length; i++) {
                        dims[i] = dimsSeg.getAtIndex(ValueLayout.JAVA_LONG, i);
                    }

                    // Read data.
                    // Read data into contiguous MemorySegment
                    int refSize           = H5R_REF_BUF_SIZE();
                    MemorySegment refData = arena.allocate(refSize * dims[0]);
                    H5Aread(attribute_id, H5T_STD_REF_g(), refData);

                    // Unpack references from contiguous MemorySegment
                    for (int i = 0; i < dims[0]; i++) {
                        MemorySegment.copy(refData, i * refSize, refs[i], 0, refSize);
                    }

                    // Output the data to the screen.
                    for (int indx = 0; indx < dims[0]; indx++) {
                        System.out.println(DATASETNAME + "[" + indx + "]:");
                        System.out.print("  ->");
                        // Open the referenced object.
                        try {
                            object_id = H5Ropen_object(refs[indx], H5P_DEFAULT(), H5P_DEFAULT());
                            try {
                                // Get the name - first query size
                                long name_size  = H5Iget_name(object_id, MemorySegment.NULL, 0);
                                String obj_name = null;
                                if (name_size > 0) {
                                    MemorySegment nameBuffer = arena.allocate(name_size + 1);
                                    H5Iget_name(object_id, nameBuffer, name_size + 1);
                                    obj_name = nameBuffer.getString(0);
                                }

                                region_id = H5Ropen_region(refs[indx], H5P_DEFAULT(), H5P_DEFAULT());
                                if ((object_id >= 0) && (region_id >= 0)) {
                                    try {
                                        long reg_npoints = H5Sget_select_npoints(region_id);
                                        long[] dims2     = new long[1];
                                        dims2[0]         = (int)reg_npoints;
                                        dataspace_id     = H5Screate_simple(
                                            1, arena.allocateFrom(ValueLayout.JAVA_LONG, dims2),
                                            MemorySegment.NULL);

                                        // Read data.
                                        MemorySegment refbuf = arena.allocate((int)reg_npoints + 1);
                                        H5Dread(object_id, H5T_STD_I8LE_g(), dataspace_id, region_id,
                                                H5P_DEFAULT(), refbuf);
                                        refbuf.set(ValueLayout.JAVA_BYTE, (int)reg_npoints, (byte)0);
                                        str_data = new StringBuffer(refbuf.getString(0).trim());

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
                                    H5Sclose(region_id);
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
                                H5Dclose(object_id);
                            }
                            catch (Exception ex) {
                            }
                        }
                    } // end for
                }
                catch (Exception e4) {
                    e4.printStackTrace();
                }
                finally {
                    try {
                        H5Sclose(dataspace_id);
                        for (int indx = 0; indx < dims[0]; indx++)
                            H5Rdestroy(refs[indx]);
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
                    H5Aclose(attribute_id);
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
                H5Fclose(file_id);
            }
            catch (Exception e2) {
            }
        }
    }

    public static void main(String[] args)
    {

        try (Arena arena = Arena.ofConfined()) {
            H5Ex_T_RegionReferenceAttribute.writeRegRef(arena);
            H5Ex_T_RegionReferenceAttribute.readRegRef(arena);
        }
    }
}
