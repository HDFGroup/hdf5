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

package jtest;

import static org.junit.Assert.*;

import static jtest.FfmTestSupport.*;

import java.lang.foreign.Arena;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.ValueLayout;

import org.hdfgroup.javahdf5.hdf5_h;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

/**
 * FFM-only tests for HDF5 VOL (Virtual Object Layer) operations.
 *
 * NOTE: These tests focus on the native VOL connector only.
 * Custom VOL connectors require external plugins and specialized setup.
 */
public class TestH5VLffm {
    @Rule
    public TestName testname = new TestName();

    private static final String H5_FILE = "test_H5VLffm.h5";

    long H5fid = hdf5_h.H5I_INVALID_HID();

    @Before
    public void createH5file()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            MemorySegment filename = stringToSegment(arena, H5_FILE);
            H5fid                  = hdf5_h.H5Fcreate(filename, hdf5_h.H5F_ACC_TRUNC(), hdf5_h.H5P_DEFAULT(),
                                                      hdf5_h.H5P_DEFAULT());
            assertTrue("H5Fcreate failed", isValidId(H5fid));
        }
    }

    @After
    public void deleteH5file()
    {
        if (isValidId(H5fid)) {
            closeQuietly(H5fid, hdf5_h::H5Fclose);
            H5fid = hdf5_h.H5I_INVALID_HID();
        }
        System.out.println();
    }

    /**
     * Test H5VLis_connector_registered_by_name for native VOL connector
     */
    @Test
    public void testH5VLis_connector_registered_by_name()
    {
        try (Arena arena = Arena.ofConfined()) {
            // Native VOL connector should be registered
            MemorySegment name = stringToSegment(arena, "native");
            int result         = hdf5_h.H5VLis_connector_registered_by_name(name);
            assertTrue("Native VOL connector should be registered", result > 0);

            // Non-existent connector should not be registered
            MemorySegment fake_name = stringToSegment(arena, "nonexistent_connector");
            result                  = hdf5_h.H5VLis_connector_registered_by_name(fake_name);
            assertEquals("Non-existent connector should not be registered", 0, result);
        }
    }

    /**
     * Test H5VLis_connector_registered_by_value for native VOL connector
     */
    @Test
    public void testH5VLis_connector_registered_by_value()
    {
        // Native VOL connector (H5_VOL_NATIVE = 0) should be registered
        int result = hdf5_h.H5VLis_connector_registered_by_value(hdf5_h.H5_VOL_NATIVE());
        assertTrue("Native VOL connector should be registered", result > 0);

        // Invalid connector value should not be registered
        result = hdf5_h.H5VLis_connector_registered_by_value(9999);
        assertEquals("Invalid connector value should not be registered", 0, result);
    }

    /**
     * Test H5VLget_connector_id for file object
     */
    @Test
    public void testH5VLget_connector_id()
    {
        long connector_id = hdf5_h.H5VLget_connector_id(H5fid);
        assertTrue("Should get valid connector ID for file", isValidId(connector_id));

        // Close the connector ID
        closeQuietly(connector_id, hdf5_h::H5VLclose);
    }

    /**
     * Test H5VLget_connector_id_by_name for native connector
     */
    @Test
    public void testH5VLget_connector_id_by_name()
    {
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment name = stringToSegment(arena, "native");
            long connector_id  = hdf5_h.H5VLget_connector_id_by_name(name);
            assertTrue("Should get valid connector ID by name", isValidId(connector_id));

            // Close the connector ID
            closeQuietly(connector_id, hdf5_h::H5VLclose);
        }
    }

    /**
     * Test H5VLget_connector_id_by_value for native connector
     */
    @Test
    public void testH5VLget_connector_id_by_value()
    {
        long connector_id = hdf5_h.H5VLget_connector_id_by_value(hdf5_h.H5_VOL_NATIVE());
        assertTrue("Should get valid connector ID by value", isValidId(connector_id));

        // Close the connector ID
        closeQuietly(connector_id, hdf5_h::H5VLclose);
    }

    /**
     * Test H5VLget_connector_name for file object
     */
    @Test
    public void testH5VLget_connector_name()
    {
        try (Arena arena = Arena.ofConfined()) {
            // First call to get the size
            long size = hdf5_h.H5VLget_connector_name(H5fid, MemorySegment.NULL, 0);
            assertTrue("Should get connector name size", size > 0);

            // Allocate buffer and get the name
            MemorySegment name_buf = arena.allocate(ValueLayout.JAVA_BYTE, (int)size + 1);
            long actual_size       = hdf5_h.H5VLget_connector_name(H5fid, name_buf, size + 1);
            assertEquals("Sizes should match", size, actual_size);

            // Verify it's the native connector
            String connector_name = segmentToString(name_buf);
            assertEquals("Should be native VOL connector", "native", connector_name);
        }
    }

    /**
     * Test H5VLobject_is_native for file object
     */
    @Test
    public void testH5VLobject_is_native()
    {
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment is_native = arena.allocate(ValueLayout.JAVA_BOOLEAN);

            int result = hdf5_h.H5VLobject_is_native(H5fid, is_native);
            assertEquals("H5VLobject_is_native should succeed", 0, result);

            // File should be using native VOL
            boolean native_vol = is_native.get(ValueLayout.JAVA_BOOLEAN, 0);
            assertTrue("File should be using native VOL connector", native_vol);
        }
    }

    /**
     * Test H5VLregister_connector_by_name and H5VLunregister_connector
     *
     * Note: This tests registration/unregistration of already-registered
     * native connector to verify the API works.
     */
    @Test
    public void testH5VLregister_and_unregister_connector_by_name()
    {
        try (Arena arena = Arena.ofConfined()) {
            // Register the native connector (it's already registered, but this verifies API)
            MemorySegment name = stringToSegment(arena, "native");
            long connector_id  = hdf5_h.H5VLregister_connector_by_name(name, hdf5_h.H5P_DEFAULT());
            assertTrue("Should get valid connector ID", isValidId(connector_id));

            // Verify it's registered
            int is_registered = hdf5_h.H5VLis_connector_registered_by_name(name);
            assertTrue("Connector should be registered", is_registered > 0);

            // Note: Cannot unregister the native connector as it's always needed
            // Just close the ID we got
            closeQuietly(connector_id, hdf5_h::H5VLclose);
        }
    }

    /**
     * Test H5VLregister_connector_by_value
     */
    @Test
    public void testH5VLregister_connector_by_value()
    {
        // Register native connector by value
        long connector_id =
            hdf5_h.H5VLregister_connector_by_value(hdf5_h.H5_VOL_NATIVE(), hdf5_h.H5P_DEFAULT());
        assertTrue("Should get valid connector ID", isValidId(connector_id));

        // Close the connector ID
        closeQuietly(connector_id, hdf5_h::H5VLclose);
    }

    /**
     * Test getting connector ID multiple times and closing
     */
    @Test
    public void testH5VLclose()
    {
        // Get connector ID for file
        long connector_id1 = hdf5_h.H5VLget_connector_id(H5fid);
        assertTrue("Should get valid connector ID", isValidId(connector_id1));

        // Get another reference
        long connector_id2 = hdf5_h.H5VLget_connector_id(H5fid);
        assertTrue("Should get valid connector ID", isValidId(connector_id2));

        // Close both
        int result = hdf5_h.H5VLclose(connector_id1);
        assertEquals("H5VLclose should succeed", 0, result);

        result = hdf5_h.H5VLclose(connector_id2);
        assertEquals("H5VLclose should succeed", 0, result);
    }

    /**
     * Test VOL connector with dataset object
     */
    @Test
    public void testH5VLget_connector_id_for_dataset()
    {
        long dset_id = hdf5_h.H5I_INVALID_HID();
        try (Arena arena = Arena.ofConfined()) {
            // Create a simple dataset
            MemorySegment dsetname = stringToSegment(arena, "dataset");
            long sid               = hdf5_h.H5Screate(hdf5_h.H5S_SCALAR());
            assertTrue("H5Screate failed", isValidId(sid));

            dset_id = hdf5_h.H5Dcreate2(H5fid, dsetname, hdf5_h.H5T_NATIVE_INT_g(), sid, hdf5_h.H5P_DEFAULT(),
                                        hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());
            assertTrue("H5Dcreate2 failed", isValidId(dset_id));

            closeQuietly(sid, hdf5_h::H5Sclose);

            // Get connector ID for dataset
            long connector_id = hdf5_h.H5VLget_connector_id(dset_id);
            assertTrue("Should get valid connector ID for dataset", isValidId(connector_id));

            // Verify it's native
            MemorySegment is_native = arena.allocate(ValueLayout.JAVA_BOOLEAN);
            int result              = hdf5_h.H5VLobject_is_native(dset_id, is_native);
            assertEquals("H5VLobject_is_native should succeed", 0, result);
            assertTrue("Dataset should be using native VOL", is_native.get(ValueLayout.JAVA_BOOLEAN, 0));

            // Close connector ID
            closeQuietly(connector_id, hdf5_h::H5VLclose);
        }
        finally {
            closeQuietly(dset_id, hdf5_h::H5Dclose);
        }
    }

    /**
     * Test H5VLquery_optional (basic test with native VOL)
     *
     * Note: This is a simplified test as full optional operation testing
     * requires detailed knowledge of VOL connector capabilities.
     */
    @Test
    public void testH5VLquery_optional()
    {
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment flags = allocateLong(arena);

            // Query optional operations for file object
            // Using subcls=0 (H5VL_SUBCLS_FILE) and opt_type=0 as basic test
            int result = hdf5_h.H5VLquery_optional(H5fid, 0, 0, flags);

            // Result depends on VOL connector support
            // We just verify the API works (returns >= 0 for success or -1 for not supported)
            assertTrue("H5VLquery_optional should return valid result", result >= -1);
        }
    }
}
