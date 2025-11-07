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
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

/**
 * FFM-only tests for HDF5 Filter (H5Z) operations.
 *
 * NOTE: These tests focus on built-in HDF5 filters.
 * Custom filter registration requires C-level callbacks and is not easily testable from Java FFM.
 */
public class TestH5Zffm {
    @Rule
    public TestName testname = new TestName();

    /**
     * Test H5Zfilter_avail for built-in filters
     */
    @Test
    public void testH5Zfilter_avail_deflate()
    {
        System.out.print(testname.getMethodName());

        // Test DEFLATE (gzip) filter - should always be available
        int result = hdf5_h.H5Zfilter_avail(hdf5_h.H5Z_FILTER_DEFLATE());
        assertTrue("DEFLATE filter should be available", result > 0);

        System.out.println();
    }

    /**
     * Test H5Zfilter_avail for shuffle filter
     */
    @Test
    public void testH5Zfilter_avail_shuffle()
    {
        System.out.print(testname.getMethodName());

        // Test SHUFFLE filter - should always be available
        int result = hdf5_h.H5Zfilter_avail(hdf5_h.H5Z_FILTER_SHUFFLE());
        assertTrue("SHUFFLE filter should be available", result > 0);

        System.out.println();
    }

    /**
     * Test H5Zfilter_avail for fletcher32 filter
     */
    @Test
    public void testH5Zfilter_avail_fletcher32()
    {
        System.out.print(testname.getMethodName());

        // Test FLETCHER32 filter - should always be available
        int result = hdf5_h.H5Zfilter_avail(hdf5_h.H5Z_FILTER_FLETCHER32());
        assertTrue("FLETCHER32 filter should be available", result > 0);

        System.out.println();
    }

    /**
     * Test H5Zfilter_avail for szip filter
     */
    @Test
    public void testH5Zfilter_avail_szip()
    {
        System.out.print(testname.getMethodName());

        // Test SZIP filter - may or may not be available depending on build
        int result = hdf5_h.H5Zfilter_avail(hdf5_h.H5Z_FILTER_SZIP());

        // Result should be either 1 (available) or 0 (not available), never negative
        assertTrue("SZIP filter availability check should not error", result >= 0);

        System.out.println();
    }

    /**
     * Test H5Zfilter_avail for nbit filter
     */
    @Test
    public void testH5Zfilter_avail_nbit()
    {
        System.out.print(testname.getMethodName());

        // Test NBIT filter - should always be available
        int result = hdf5_h.H5Zfilter_avail(hdf5_h.H5Z_FILTER_NBIT());
        assertTrue("NBIT filter should be available", result > 0);

        System.out.println();
    }

    /**
     * Test H5Zfilter_avail for scaleoffset filter
     */
    @Test
    public void testH5Zfilter_avail_scaleoffset()
    {
        System.out.print(testname.getMethodName());

        // Test SCALEOFFSET filter - should always be available
        int result = hdf5_h.H5Zfilter_avail(hdf5_h.H5Z_FILTER_SCALEOFFSET());
        assertTrue("SCALEOFFSET filter should be available", result > 0);

        System.out.println();
    }

    /**
     * Test H5Zfilter_avail for non-existent filter
     */
    @Test
    public void testH5Zfilter_avail_invalid()
    {
        System.out.print(testname.getMethodName());

        // Test with invalid filter ID - should return 0 (not available)
        int result = hdf5_h.H5Zfilter_avail(9999);
        assertEquals("Non-existent filter should not be available", 0, result);

        System.out.println();
    }

    /**
     * Test H5Zget_filter_info for deflate filter
     */
    @Test
    public void testH5Zget_filter_info_deflate()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            MemorySegment flagsSeg = arena.allocate(ValueLayout.JAVA_INT);

            // Get filter info for DEFLATE
            int result = hdf5_h.H5Zget_filter_info(hdf5_h.H5Z_FILTER_DEFLATE(), flagsSeg);
            assertEquals("H5Zget_filter_info should succeed", 0, result);

            int flags = flagsSeg.get(ValueLayout.JAVA_INT, 0);

            // Verify encode and decode are enabled
            int encodeEnabled = flags & hdf5_h.H5Z_FILTER_CONFIG_ENCODE_ENABLED();
            int decodeEnabled = flags & hdf5_h.H5Z_FILTER_CONFIG_DECODE_ENABLED();

            assertTrue("DEFLATE filter should support encoding", encodeEnabled != 0);
            assertTrue("DEFLATE filter should support decoding", decodeEnabled != 0);
        }

        System.out.println();
    }

    /**
     * Test H5Zget_filter_info for shuffle filter
     */
    @Test
    public void testH5Zget_filter_info_shuffle()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            MemorySegment flagsSeg = arena.allocate(ValueLayout.JAVA_INT);

            // Get filter info for SHUFFLE
            int result = hdf5_h.H5Zget_filter_info(hdf5_h.H5Z_FILTER_SHUFFLE(), flagsSeg);
            assertEquals("H5Zget_filter_info should succeed", 0, result);

            int flags = flagsSeg.get(ValueLayout.JAVA_INT, 0);

            // Verify encode and decode are enabled
            int encodeEnabled = flags & hdf5_h.H5Z_FILTER_CONFIG_ENCODE_ENABLED();
            int decodeEnabled = flags & hdf5_h.H5Z_FILTER_CONFIG_DECODE_ENABLED();

            assertTrue("SHUFFLE filter should support encoding", encodeEnabled != 0);
            assertTrue("SHUFFLE filter should support decoding", decodeEnabled != 0);
        }

        System.out.println();
    }

    /**
     * Test H5Zget_filter_info for fletcher32 filter
     */
    @Test
    public void testH5Zget_filter_info_fletcher32()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            MemorySegment flagsSeg = arena.allocate(ValueLayout.JAVA_INT);

            // Get filter info for FLETCHER32
            int result = hdf5_h.H5Zget_filter_info(hdf5_h.H5Z_FILTER_FLETCHER32(), flagsSeg);
            assertEquals("H5Zget_filter_info should succeed", 0, result);

            int flags = flagsSeg.get(ValueLayout.JAVA_INT, 0);

            // Verify encode and decode are enabled
            int encodeEnabled = flags & hdf5_h.H5Z_FILTER_CONFIG_ENCODE_ENABLED();
            int decodeEnabled = flags & hdf5_h.H5Z_FILTER_CONFIG_DECODE_ENABLED();

            assertTrue("FLETCHER32 filter should support encoding", encodeEnabled != 0);
            assertTrue("FLETCHER32 filter should support decoding", decodeEnabled != 0);
        }

        System.out.println();
    }

    /**
     * Test H5Zget_filter_info for szip filter (if available)
     */
    @Test
    public void testH5Zget_filter_info_szip()
    {
        System.out.print(testname.getMethodName());

        // Check if SZIP is available first
        int available = hdf5_h.H5Zfilter_avail(hdf5_h.H5Z_FILTER_SZIP());

        if (available > 0) {
            try (Arena arena = Arena.ofConfined()) {
                MemorySegment flagsSeg = arena.allocate(ValueLayout.JAVA_INT);

                // Get filter info for SZIP
                int result = hdf5_h.H5Zget_filter_info(hdf5_h.H5Z_FILTER_SZIP(), flagsSeg);
                assertEquals("H5Zget_filter_info should succeed", 0, result);

                int flags = flagsSeg.get(ValueLayout.JAVA_INT, 0);

                // SZIP may support only decoding, only encoding, or both
                // Just verify flags are set to something valid
                assertTrue("SZIP filter should have some capabilities", flags >= 0);
            }
        }

        System.out.println();
    }

    /**
     * Test H5Zget_filter_info for nbit filter
     */
    @Test
    public void testH5Zget_filter_info_nbit()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            MemorySegment flagsSeg = arena.allocate(ValueLayout.JAVA_INT);

            // Get filter info for NBIT
            int result = hdf5_h.H5Zget_filter_info(hdf5_h.H5Z_FILTER_NBIT(), flagsSeg);
            assertEquals("H5Zget_filter_info should succeed", 0, result);

            int flags = flagsSeg.get(ValueLayout.JAVA_INT, 0);

            // Verify encode and decode are enabled
            int encodeEnabled = flags & hdf5_h.H5Z_FILTER_CONFIG_ENCODE_ENABLED();
            int decodeEnabled = flags & hdf5_h.H5Z_FILTER_CONFIG_DECODE_ENABLED();

            assertTrue("NBIT filter should support encoding", encodeEnabled != 0);
            assertTrue("NBIT filter should support decoding", decodeEnabled != 0);
        }

        System.out.println();
    }

    /**
     * Test H5Zget_filter_info for scaleoffset filter
     */
    @Test
    public void testH5Zget_filter_info_scaleoffset()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            MemorySegment flagsSeg = arena.allocate(ValueLayout.JAVA_INT);

            // Get filter info for SCALEOFFSET
            int result = hdf5_h.H5Zget_filter_info(hdf5_h.H5Z_FILTER_SCALEOFFSET(), flagsSeg);
            assertEquals("H5Zget_filter_info should succeed", 0, result);

            int flags = flagsSeg.get(ValueLayout.JAVA_INT, 0);

            // Verify encode and decode are enabled
            int encodeEnabled = flags & hdf5_h.H5Z_FILTER_CONFIG_ENCODE_ENABLED();
            int decodeEnabled = flags & hdf5_h.H5Z_FILTER_CONFIG_DECODE_ENABLED();

            assertTrue("SCALEOFFSET filter should support encoding", encodeEnabled != 0);
            assertTrue("SCALEOFFSET filter should support decoding", decodeEnabled != 0);
        }

        System.out.println();
    }

    /**
     * Test H5Zget_filter_info with invalid filter ID
     */
    @Test
    public void testH5Zget_filter_info_invalid()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            MemorySegment flagsSeg = arena.allocate(ValueLayout.JAVA_INT);

            // Try to get info for non-existent filter
            int result = hdf5_h.H5Zget_filter_info(9999, flagsSeg);

            // Should return error (negative value)
            assertTrue("Getting info for invalid filter should fail", result < 0);
        }

        System.out.println();
    }

    /**
     * Test filter ID constants
     */
    @Test
    public void testH5Z_filter_constants()
    {
        System.out.print(testname.getMethodName());

        // Verify standard filter IDs have expected values
        assertEquals("H5Z_FILTER_NONE should be 0", 0, hdf5_h.H5Z_FILTER_NONE());
        assertEquals("H5Z_FILTER_DEFLATE should be 1", 1, hdf5_h.H5Z_FILTER_DEFLATE());
        assertEquals("H5Z_FILTER_SHUFFLE should be 2", 2, hdf5_h.H5Z_FILTER_SHUFFLE());
        assertEquals("H5Z_FILTER_FLETCHER32 should be 3", 3, hdf5_h.H5Z_FILTER_FLETCHER32());
        assertEquals("H5Z_FILTER_SZIP should be 4", 4, hdf5_h.H5Z_FILTER_SZIP());
        assertEquals("H5Z_FILTER_NBIT should be 5", 5, hdf5_h.H5Z_FILTER_NBIT());
        assertEquals("H5Z_FILTER_SCALEOFFSET should be 6", 6, hdf5_h.H5Z_FILTER_SCALEOFFSET());

        // Verify reserved value
        assertEquals("H5Z_FILTER_RESERVED should be 256", 256, hdf5_h.H5Z_FILTER_RESERVED());

        System.out.println();
    }

    /**
     * Test filter config flag constants
     */
    @Test
    public void testH5Z_filter_config_flags()
    {
        System.out.print(testname.getMethodName());

        // Verify config flag values
        int encodeFlag = hdf5_h.H5Z_FILTER_CONFIG_ENCODE_ENABLED();
        int decodeFlag = hdf5_h.H5Z_FILTER_CONFIG_DECODE_ENABLED();

        assertEquals("H5Z_FILTER_CONFIG_ENCODE_ENABLED should be 0x0001", 0x0001, encodeFlag);
        assertEquals("H5Z_FILTER_CONFIG_DECODE_ENABLED should be 0x0002", 0x0002, decodeFlag);

        System.out.println();
    }

    /**
     * Test all built-in filters are available
     */
    @Test
    public void testH5Z_all_builtin_filters()
    {
        System.out.print(testname.getMethodName());

        // Test all built-in filters (except SZIP which may not be available)
        int[] requiredFilters = {hdf5_h.H5Z_FILTER_DEFLATE(), hdf5_h.H5Z_FILTER_SHUFFLE(),
                                 hdf5_h.H5Z_FILTER_FLETCHER32(), hdf5_h.H5Z_FILTER_NBIT(),
                                 hdf5_h.H5Z_FILTER_SCALEOFFSET()};

        for (int filter : requiredFilters) {
            int available = hdf5_h.H5Zfilter_avail(filter);
            assertTrue("Filter " + filter + " should be available", available > 0);
        }

        System.out.println();
    }
}
