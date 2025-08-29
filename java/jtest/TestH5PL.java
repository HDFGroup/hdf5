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

package test;

import static org.hdfgroup.javahdf5.hdf5_h.*;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.lang.foreign.Arena;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.ValueLayout;

import org.hdfgroup.javahdf5.*;
import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

public class TestH5PL {
    @Rule
    public TestName testname                    = new TestName();
    private static String FILENAME              = "h5_dlopenChunk.h5";
    private static String DATASETNAME           = "DS1";
    private static final int DIM_X              = 6;
    private static final int DIM_Y              = 8;
    private static final int CHUNK_X            = 4;
    private static final int CHUNK_Y            = 4;
    private static final int RANK               = 2;
    private static final int NDIMS              = 2;
    private static final int H5Z_FILTER_DYNLIB4 = 260;

    @Before
    public void checkOpenIDs()
    {
        System.out.print(testname.getMethodName());
    }
    @After
    public void nextTestName()
    {
        System.out.println();
    }

    @Test
    public void TestH5PLplugins()
    {
        try {
            int plugin_flags;
            try (Arena offHeap = Arena.ofConfined()) {
                // Allocate memory to store the integers
                MemorySegment plugin_flags_ptr = offHeap.allocate(ValueLayout.JAVA_INT);
                H5PLget_loading_state(plugin_flags_ptr);
                plugin_flags = plugin_flags_ptr.get(ValueLayout.JAVA_INT, 0);
            }
            assertTrue("H5PLget_loading_state: " + plugin_flags, plugin_flags == H5PL_ALL_PLUGIN());
            int new_setting = plugin_flags & ~H5PL_FILTER_PLUGIN();
            H5PLset_loading_state(new_setting);
            int changed_flags;
            try (Arena offHeap = Arena.ofConfined()) {
                // Allocate memory to store the integers
                MemorySegment plugin_flags_ptr = offHeap.allocate(ValueLayout.JAVA_INT);
                H5PLget_loading_state(plugin_flags_ptr);
                changed_flags = plugin_flags_ptr.get(ValueLayout.JAVA_INT, 0);
            }
            assertTrue("H5PLget_loading_state: " + changed_flags, changed_flags == new_setting);
            H5PLset_loading_state(plugin_flags);
            try (Arena offHeap = Arena.ofConfined()) {
                // Allocate memory to store the integers
                MemorySegment plugin_flags_ptr = offHeap.allocate(ValueLayout.JAVA_INT);
                H5PLget_loading_state(plugin_flags_ptr);
                changed_flags = plugin_flags_ptr.get(ValueLayout.JAVA_INT, 0);
            }
            assertTrue("H5PLget_loading_state: " + changed_flags, changed_flags == H5PL_ALL_PLUGIN());
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5PLplugins " + err);
        }
    }

    @Test
    public void TestH5PLpaths()
    {
        try {
            // Get the original number of paths
            int nStartPaths = 0;

            int nPaths     = nStartPaths; /* # paths from H5PLSize()      */
            int nTruePaths = nStartPaths; /* What the # paths should be   */
            int index;                    /* Path table index             */
            String path;                  /* Path from H5PLget()          */

            // CURRENT number of paths
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the array bytes
                MemorySegment num_paths_segment = arena.allocateFrom(ValueLayout.JAVA_INT, nStartPaths);
                H5PLsize(num_paths_segment);
            }

            // APPEND a path and ensure it was added correctly
            String pathAppend = "path_append";
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment path_name_segment = arena.allocateFrom(pathAppend);
                H5PLappend(path_name_segment);
            }

            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the array bytes
                MemorySegment num_paths_segment = arena.allocateFrom(ValueLayout.JAVA_INT, nPaths);
                H5PLsize(num_paths_segment);
            }
            nTruePaths++;
            assertTrue("# paths should be " + nTruePaths + " but was " + nPaths, nTruePaths == nPaths);

            index = nTruePaths - 1;
            /* Get the length of the name */
            long buf_size = H5PLget(index, null, 0);
            assertTrue("length of the name should be positive", buf_size > 0);
            /* Allocate buffer for the name */
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment path_name_segment = arena.allocate(buf_size + 1);
                H5PLget(index, path_name_segment, buf_size + 1);
                path = path_name_segment.getString(0);
            }
            assertTrue("Path should be " + pathAppend + " but was " + path,
                       path.compareToIgnoreCase(pathAppend) == 0);

            // PREPEND a path and ensure it was added correctly
            String pathPrepend = "path_prepend";
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment path_name_segment = arena.allocateFrom(pathPrepend);
                H5PLprepend(path_name_segment);
            }

            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the array bytes
                MemorySegment num_paths_segment = arena.allocateFrom(ValueLayout.JAVA_INT, nPaths);
                H5PLsize(num_paths_segment);
            }
            nTruePaths++;
            assertTrue("# paths should be " + nTruePaths + " but was " + nPaths, nTruePaths == nPaths);

            index = 0;
            /* Get the length of the name */
            buf_size = H5PLget(index, null, 0);
            assertTrue("length of the name should be positive", buf_size > 0);
            /* Allocate buffer for the name */
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment path_name_segment = arena.allocate(buf_size + 1);
                H5PLget(index, path_name_segment, buf_size + 1);
                path = path_name_segment.getString(0);
            }
            assertTrue("Path should be " + pathPrepend + " but was " + path,
                       path.compareToIgnoreCase(pathPrepend) == 0);

            // INSERT a path and ensure it was added correctly
            // Inserting at the index == # of start paths ensures we're in the middle
            String pathInsert = "path_insert";
            index             = nStartPaths;
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment path_name_segment = arena.allocateFrom(pathInsert);
                H5PLinsert(path_name_segment, index);
            }

            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the array bytes
                MemorySegment num_paths_segment = arena.allocateFrom(ValueLayout.JAVA_INT, nPaths);
                H5PLsize(num_paths_segment);
            }
            nTruePaths++;
            assertTrue("# paths should be " + nTruePaths + " but was " + nPaths, nTruePaths == nPaths);

            /* Get the length of the name */
            buf_size = H5PLget(index, null, 0);
            assertTrue("length of the name should be positive", buf_size > 0);
            /* Allocate buffer for the name */
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment path_name_segment = arena.allocate(buf_size + 1);
                H5PLget(index, path_name_segment, buf_size + 1);
                path = path_name_segment.getString(0);
            }
            assertTrue("Path should be " + pathInsert + " but was " + path,
                       path.compareToIgnoreCase(pathInsert) == 0);

            // REPLACE the path we just added and ensure it updated correctly
            String pathReplace = "path_replace";
            index              = nStartPaths;
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment path_name_segment = arena.allocateFrom(pathReplace);
                H5PLreplace(path_name_segment, index);
            }

            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the array bytes
                MemorySegment num_paths_segment = arena.allocateFrom(ValueLayout.JAVA_INT, nPaths);
                H5PLsize(num_paths_segment);
            }
            assertTrue("# paths should be " + nTruePaths + " but was " + nPaths, nTruePaths == nPaths);

            /* Get the length of the name */
            buf_size = H5PLget(index, null, 0);
            assertTrue("length of the name should be positive", buf_size > 0);
            /* Allocate buffer for the name */
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment path_name_segment = arena.allocate(buf_size + 1);
                H5PLget(index, path_name_segment, buf_size + 1);
                path = path_name_segment.getString(0);
            }
            assertTrue("Path should be " + pathReplace + " but was " + path,
                       path.compareToIgnoreCase(pathReplace) == 0);

            // REMOVE the path we just replaced and check that the table was compacted
            // The (index+1) path should move down to fill the space when the path is removed.
            index             = nStartPaths;
            String pathRemove = null;
            /* Get the length of the name */
            buf_size = H5PLget(index + 1, null, 0);
            assertTrue("length of the name should be positive", buf_size > 0);
            /* Allocate buffer for the name */
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment path_name_segment = arena.allocate(buf_size + 1);
                H5PLget(index, path_name_segment, buf_size + 1);
                pathRemove = path_name_segment.getString(0);
            }
            H5PLremove(index);

            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the array bytes
                MemorySegment num_paths_segment = arena.allocateFrom(ValueLayout.JAVA_INT, nPaths);
                H5PLsize(num_paths_segment);
            }
            nTruePaths--;
            assertTrue("# paths should be " + nTruePaths + " but was " + nPaths, nTruePaths == nPaths);

            /* Get the length of the name */
            buf_size = H5PLget(index, null, 0);
            assertTrue("length of the name should be positive", buf_size > 0);
            /* Allocate buffer for the name */
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment path_name_segment = arena.allocate(buf_size + 1);
                H5PLget(index, path_name_segment, buf_size + 1);
                path = path_name_segment.getString(0);
            }
            assertTrue("Path should be " + pathRemove + " but was " + path,
                       path.compareToIgnoreCase(pathRemove) == 0);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5PLpaths " + err);
        }
    }

    @Ignore
    public void TestH5PLdlopen()
    {
        long file_id      = H5I_INVALID_HID();
        long filespace_id = H5I_INVALID_HID();
        long dataset_id   = H5I_INVALID_HID();
        long fapl_id      = H5I_INVALID_HID();
        long dcpl_id      = H5I_INVALID_HID();
        try {
            int[] cd_values    = {9, 0, 0, 0};
            int[] libversion   = {0, 0, 0};
            long[] dims        = {DIM_X, DIM_Y};
            long[] chunk_dims  = {CHUNK_X, CHUNK_Y};
            int[] dset_data    = new int[DIM_X * DIM_Y];
            int[] mdc_nelmts   = {0};
            long[] rdcc_nelmts = {0};
            long[] rdcc_nbytes = {0};
            double[] rdcc_w0   = {0};

            // Initialize data to "1", to make it easier to see the selections.
            for (int indx = 0; indx < DIM_X; indx++)
                for (int jndx = 0; jndx < DIM_Y; jndx++)
                    dset_data[indx * DIM_Y + jndx] = 1;

            // Create a new file using default properties.
            try {
                try (Arena arena = Arena.ofConfined()) {
                    // Allocate a MemorySegment to hold the string bytes
                    MemorySegment filename_segment = arena.allocateFrom(FILENAME);
                    file_id = H5Fcreate(filename_segment, H5F_ACC_TRUNC(), H5P_DEFAULT(), H5P_DEFAULT());
                }
                catch (Throwable err) {
                    err.printStackTrace();
                    fail("Arena: " + err);
                }
            }
            catch (Exception e) {
                e.printStackTrace();
                fail("TestH5PLdlopen H5Fcreate:" + e);
            }

            // Create dataspace. Setting maximum size to NULL sets the maximum
            // size to be the current size.
            try {
                try (Arena arena = Arena.ofConfined()) {
                    // Allocate a MemorySegment to hold the dims bytes
                    MemorySegment dims_segment = MemorySegment.ofArray(dims);
                    filespace_id               = H5Screate_simple(RANK, dims_segment, null);
                }
                catch (Throwable err) {
                    err.printStackTrace();
                    fail("Arena: " + err);
                }
            }
            catch (Exception e) {
                e.printStackTrace();
                fail("TestH5PLdlopen H5Screate_simple:" + e);
            }

            // Create the dataset creation property list.
            try {
                dcpl_id = H5Pcreate(H5P_CLS_DATASET_CREATE_ID_g());
            }
            catch (Exception e) {
                e.printStackTrace();
                fail("TestH5PLdlopen H5Pcreate:" + e);
            }

            // Set the chunk size.
            try {
                if (dcpl_id >= 0)
                    try (Arena arena = Arena.ofConfined()) {
                        // Allocate a MemorySegment to hold the array bytes
                        MemorySegment chunk_dims_segment = MemorySegment.ofArray(chunk_dims);
                        H5Pset_chunk(dcpl_id, NDIMS, chunk_dims_segment);
                    }
            }
            catch (Exception e) {
                e.printStackTrace();
                fail("TestH5PLdlopen H5Pset_chunk:" + e);
            }

            try {
                try (Arena arena = Arena.ofConfined()) {
                    // Allocate a MemorySegment to hold the array bytes
                    MemorySegment majnum_segment = arena.allocateFrom(ValueLayout.JAVA_INT, libversion[0]);
                    MemorySegment minnum_segment = arena.allocateFrom(ValueLayout.JAVA_INT, libversion[1]);
                    MemorySegment relnum_segment = arena.allocateFrom(ValueLayout.JAVA_INT, libversion[2]);
                    H5get_libversion(majnum_segment, minnum_segment, relnum_segment);
                }
                catch (Throwable err) {
                    fail("H5get_libversion: " + err);
                }
                cd_values[1] = libversion[0];
                cd_values[2] = libversion[1];
                cd_values[3] = libversion[2];
                if (dcpl_id >= 0)
                    try (Arena arena = Arena.ofConfined()) {
                        // Allocate a MemorySegment to hold the array bytes
                        MemorySegment cd_values_segment = MemorySegment.ofArray(cd_values);
                        H5Pset_filter(dcpl_id, H5Z_FILTER_DYNLIB4, H5Z_FLAG_MANDATORY(), 4,
                                      cd_values_segment);
                    }
            }
            catch (Exception e) {
                e.printStackTrace();
                fail("TestH5PLdlopen H5Pset_filter:" + e);
            }

            // Create the chunked dataset.
            try {
                if ((file_id >= 0) && (filespace_id >= 0) && (dcpl_id >= 0))
                    try (Arena arena = Arena.ofConfined()) {
                        // Allocate a MemorySegment to hold the string bytes
                        MemorySegment name_segment = arena.allocateFrom(DATASETNAME);
                        dataset_id = H5Dcreate2(file_id, name_segment, H5T_NATIVE_INT_g(), filespace_id,
                                                H5P_DEFAULT(), dcpl_id, H5P_DEFAULT());
                    }
                    catch (Throwable err) {
                        err.printStackTrace();
                        fail("Arena: " + err);
                    }
            }
            catch (Exception e) {
                e.printStackTrace();
                fail("TestH5PLdlopen H5Dcreate:" + e);
            }

            try {
                if (dataset_id >= 0)
                    try (Arena arena = Arena.ofConfined()) {
                        // Allocate a MemorySegment to hold the array bytes
                        MemorySegment dset_data_segment = MemorySegment.ofArray(dset_data);
                        H5Dwrite(dataset_id, H5T_NATIVE_INT_g(), H5S_ALL(), H5S_ALL(), H5S_ALL(),
                                 dset_data_segment);
                    }
            }
            catch (Exception e) {
                e.printStackTrace();
                fail("TestH5PLdlopen H5Dwrite:" + e);
            }
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5PLdlopen " + err);
        }
        finally {
            // End access to the dataset and release resources used by it.
            if (dcpl_id >= 0)
                try {
                    H5Pclose_class(dcpl_id);
                }
                catch (Throwable err) {
                }
            if (dataset_id >= 0)
                try {
                    H5Dclose(dataset_id);
                }
                catch (Throwable err) {
                }
            if (filespace_id >= 0)
                try {
                    H5Sclose(filespace_id);
                }
                catch (Throwable err) {
                }
            if (file_id >= 0)
                try {
                    H5Fclose(file_id);
                }
                catch (Throwable err) {
                }
        }
    }
}
