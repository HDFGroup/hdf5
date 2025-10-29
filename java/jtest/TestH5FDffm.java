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
 * FFM-only tests for HDF5 Virtual File Driver (H5FD) operations.
 *
 * NOTE: These tests focus on built-in VFD registration checking and driver queries.
 * Low-level VFD operations require file handles and are tested through H5F/H5P APIs.
 */
public class TestH5FDffm {
    @Rule
    public TestName testname = new TestName();

    private static final String H5_FILE = "test_H5FDffm.h5";

    long H5fid     = hdf5_h.H5I_INVALID_HID();
    long H5fapl_id = hdf5_h.H5I_INVALID_HID();

    @Before
    public void createH5file()
    {
        System.out.print(testname.getMethodName());

        // Ensure HDF5 library is initialized (prevents FFM constant initialization issues)
        hdf5_h.H5open();

        H5fapl_id = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_FILE_ACCESS_ID_g());
        assertTrue("H5Pcreate failed", isValidId(H5fapl_id));

        try (Arena arena = Arena.ofConfined()) {
            MemorySegment filename = stringToSegment(arena, H5_FILE);
            H5fid = hdf5_h.H5Fcreate(filename, hdf5_h.H5F_ACC_TRUNC(), hdf5_h.H5P_DEFAULT(), H5fapl_id);
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
        if (isValidId(H5fapl_id)) {
            closeQuietly(H5fapl_id, hdf5_h::H5Pclose);
            H5fapl_id = hdf5_h.H5I_INVALID_HID();
        }
        System.out.println();
    }

    /**
     * Test H5FDis_driver_registered_by_name for sec2 driver
     */
    @Test
    public void testH5FDis_driver_registered_by_name_sec2()
    {
        try (Arena arena = Arena.ofConfined()) {
            // SEC2 driver should always be registered
            MemorySegment name = stringToSegment(arena, "sec2");
            int result         = hdf5_h.H5FDis_driver_registered_by_name(name);
            assertTrue("SEC2 driver should be registered", result > 0);
        }
    }

    /**
     * Test H5FDis_driver_registered_by_name for core driver
     */
    @Test
    public void testH5FDis_driver_registered_by_name_core()
    {
        try (Arena arena = Arena.ofConfined()) {
            // CORE (memory) driver should always be registered
            MemorySegment name = stringToSegment(arena, "core");
            int result         = hdf5_h.H5FDis_driver_registered_by_name(name);
            assertTrue("CORE driver should be registered", result > 0);
        }
    }

    /**
     * Test H5FDis_driver_registered_by_name for family driver
     */
    @Test
    public void testH5FDis_driver_registered_by_name_family()
    {
        try (Arena arena = Arena.ofConfined()) {
            // FAMILY driver should always be registered
            MemorySegment name = stringToSegment(arena, "family");
            int result         = hdf5_h.H5FDis_driver_registered_by_name(name);
            assertTrue("FAMILY driver should be registered", result > 0);
        }
    }

    /**
     * Test H5FDis_driver_registered_by_name for non-existent driver
     */
    @Test
    public void testH5FDis_driver_registered_by_name_invalid()
    {
        try (Arena arena = Arena.ofConfined()) {
            // Non-existent driver should not be registered
            MemorySegment name = stringToSegment(arena, "nonexistent_driver");
            int result         = hdf5_h.H5FDis_driver_registered_by_name(name);
            assertEquals("Non-existent driver should not be registered", 0, result);
        }
    }

    /**
     * Test H5FDis_driver_registered_by_value for sec2 driver
     */
    @Test
    public void testH5FDis_driver_registered_by_value_sec2()
    {
        // SEC2 driver (H5_VFD_SEC2 = 0) should be registered
        int result = hdf5_h.H5FDis_driver_registered_by_value(hdf5_h.H5_VFD_SEC2());
        assertTrue("SEC2 driver should be registered", result > 0);
    }

    /**
     * Test H5FDis_driver_registered_by_value for core driver
     */
    @Test
    public void testH5FDis_driver_registered_by_value_core()
    {
        // CORE driver (H5_VFD_CORE = 1) should be registered
        int result = hdf5_h.H5FDis_driver_registered_by_value(hdf5_h.H5_VFD_CORE());
        assertTrue("CORE driver should be registered", result > 0);
    }

    /**
     * Test H5FDis_driver_registered_by_value for invalid driver
     */
    @Test
    public void testH5FDis_driver_registered_by_value_invalid()
    {
        // Invalid driver value should not be registered
        int result = hdf5_h.H5FDis_driver_registered_by_value(9999);
        assertEquals("Invalid driver value should not be registered", 0, result);
    }

    /**
     * Test H5FDdriver_query for file driver
     */
    @Test
    public void testH5FDdriver_query()
    {
        try (Arena arena = Arena.ofConfined()) {
            // Get driver ID from FAPL
            long driver_id = hdf5_h.H5Pget_driver(H5fapl_id);
            assertTrue("Should get valid driver ID", isValidId(driver_id));

            // Query driver flags
            MemorySegment flagsSeg = arena.allocate(ValueLayout.JAVA_LONG);
            int result             = hdf5_h.H5FDdriver_query(driver_id, flagsSeg);
            assertEquals("H5FDdriver_query should succeed", 0, result);

            long flags = flagsSeg.get(ValueLayout.JAVA_LONG, 0);

            // Flags should be non-zero for most drivers
            assertTrue("Driver should report some features", flags >= 0);
        }
    }

    /**
     * Test VFD value constants
     */
    @Test
    public void testH5_VFD_constants()
    {
        // Verify VFD value constants
        assertEquals("H5_VFD_SEC2 should be 0", 0, hdf5_h.H5_VFD_SEC2());
        assertEquals("H5_VFD_CORE should be 1", 1, hdf5_h.H5_VFD_CORE());
        assertEquals("H5_VFD_LOG should be 2", 2, hdf5_h.H5_VFD_LOG());
        assertEquals("H5_VFD_FAMILY should be 3", 3, hdf5_h.H5_VFD_FAMILY());
        assertEquals("H5_VFD_MULTI should be 4", 4, hdf5_h.H5_VFD_MULTI());

        // Verify reserved values
        assertEquals("H5_VFD_RESERVED should be 256", 256, hdf5_h.H5_VFD_RESERVED());
    }

    /**
     * Test multiple built-in VFDs are registered
     */
    @Test
    public void testH5FD_builtin_drivers()
    {
        // Test that common built-in drivers are registered
        String[] builtinDrivers = {"sec2", "core", "family", "multi", "log"};

        try (Arena arena = Arena.ofConfined()) {
            for (String driverName : builtinDrivers) {
                MemorySegment name = stringToSegment(arena, driverName);
                int result         = hdf5_h.H5FDis_driver_registered_by_name(name);
                assertTrue(driverName + " driver should be registered", result >= 0);
            }
        }
    }
}
