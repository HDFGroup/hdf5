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

import java.lang.foreign.Arena;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.ValueLayout;

/**
 * <p>
 * Title: HDF Native Package (Java) Example
 * </p>
 * <p>
 * Description: This example shows how to create an empty HDF5 file using the
 * "HDF Native Package (Java)". If the file (H5FileCreate.h5) already exists, it
 * will be truncated to zero length.
 * </p>
 */
public class HDF5FileCreate {
    // The name of the file we'll create.
    private static String fname = "HDF5FileCreate.h5";

    private static void CreateFile(Arena arena)
    {
        long file_id = H5I_INVALID_HID();

        // Create a new file using default properties.
        try {
            file_id = H5Fcreate(arena.allocateFrom(fname), H5F_ACC_TRUNC(), H5P_DEFAULT(), H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
            System.err.println("Failed to create file:" + fname);
            return;
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
            HDF5FileCreate.CreateFile(arena);
        }
    }
}
