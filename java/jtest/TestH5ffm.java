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
 * FFM-only tests for general HDF5 library operations (H5public.h APIs).
 *
 * This test class covers general library initialization, cleanup, version checking,
 * memory management, and library configuration APIs.
 */
public class TestH5ffm {
    @Rule
    public TestName testname = new TestName();

    @Before
    public void setup()
    {
        System.out.print(testname.getMethodName());
        // Ensure library is initialized
        hdf5_h.H5open();
    }

    @After
    public void cleanup()
    {
        System.out.println();
    }

    // ================================
    // Library Initialization and Cleanup
    // ================================

    @Test
    public void testH5open()
    {
        // H5open is idempotent - calling multiple times should succeed
        int result = hdf5_h.H5open();
        assertTrue("H5open should succeed", isSuccess(result));

        result = hdf5_h.H5open();
        assertTrue("H5open should be idempotent", isSuccess(result));
    }

    @Test
    public void testH5close()
    {
        // H5close decrements reference count but doesn't actually close if other references exist
        // This test just verifies the API is callable
        int result = hdf5_h.H5close();
        assertTrue("H5close should return valid code", result >= -1);

        // Re-open to ensure library is available for other tests
        hdf5_h.H5open();
    }

    @Test
    public void testH5garbage_collect()
    {
        int result = hdf5_h.H5garbage_collect();
        assertTrue("H5garbage_collect should succeed", isSuccess(result));
    }

    @Test
    public void testH5dont_atexit()
    {
        // Note: This affects cleanup behavior, test just verifies API is callable
        int result = hdf5_h.H5dont_atexit();
        assertTrue("H5dont_atexit should succeed", isSuccess(result));
    }

    // ================================
    // Version Information
    // ================================

    @Test
    public void testH5get_libversion()
    {
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment majnum = allocateInt(arena);
            MemorySegment minnum = allocateInt(arena);
            MemorySegment relnum = allocateInt(arena);

            int result = hdf5_h.H5get_libversion(majnum, minnum, relnum);
            assertTrue("H5get_libversion failed", isSuccess(result));

            int major   = getInt(majnum);
            int minor   = getInt(minnum);
            int release = getInt(relnum);

            assertTrue("Major version should be >= 1", major >= 1);
            assertTrue("Minor version should be >= 0", minor >= 0);
            assertTrue("Release version should be >= 0", release >= 0);

            System.out.print(" [HDF5 " + major + "." + minor + "." + release + "]");
        }
    }

    @Test
    public void testH5check_version()
    {
        try (Arena arena = Arena.ofConfined()) {
            // Get current version
            MemorySegment majnum = allocateInt(arena);
            MemorySegment minnum = allocateInt(arena);
            MemorySegment relnum = allocateInt(arena);

            hdf5_h.H5get_libversion(majnum, minnum, relnum);

            int major   = getInt(majnum);
            int minor   = getInt(minnum);
            int release = getInt(relnum);

            // Check against current version should succeed
            int result = hdf5_h.H5check_version(major, minor, release);
            assertTrue("H5check_version with correct version should succeed", isSuccess(result));
        }
    }

    // ================================
    // Library Status Queries
    // ================================

    @Test
    public void testH5is_library_threadsafe()
    {
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment isThreadsafe = arena.allocate(ValueLayout.JAVA_BOOLEAN);

            int result = hdf5_h.H5is_library_threadsafe(isThreadsafe);
            assertTrue("H5is_library_threadsafe failed", isSuccess(result));

            boolean threadsafe = isThreadsafe.get(ValueLayout.JAVA_BOOLEAN, 0);
            // Value can be true or false - just verify we got a valid result
            System.out.print(" [threadsafe=" + threadsafe + "]");
        }
    }

    @Test
    public void testH5is_library_terminating()
    {
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment isTerminating = arena.allocate(ValueLayout.JAVA_BOOLEAN);

            int result = hdf5_h.H5is_library_terminating(isTerminating);
            assertTrue("H5is_library_terminating failed", isSuccess(result));

            boolean terminating = isTerminating.get(ValueLayout.JAVA_BOOLEAN, 0);
            // During normal operation, should not be terminating
            assertFalse("Library should not be terminating during tests", terminating);
        }
    }

    // ================================
    // Memory Management
    // ================================

    @Test
    public void testH5allocate_memory()
    {
        try (Arena arena = Arena.ofConfined()) {
            // Allocate 1KB of memory
            long size         = 1024;
            MemorySegment mem = hdf5_h.H5allocate_memory(size, false);
            assertNotNull("H5allocate_memory should return non-null", mem);
            assertFalse("Allocated memory should not be NULL segment", mem.equals(MemorySegment.NULL));

            // Free the memory
            int result = hdf5_h.H5free_memory(mem);
            assertTrue("H5free_memory should succeed", isSuccess(result));
        }
    }

    @Test
    public void testH5allocate_memory_cleared()
    {
        try (Arena arena = Arena.ofConfined()) {
            // Allocate 1KB of cleared memory
            long size         = 1024;
            MemorySegment mem = hdf5_h.H5allocate_memory(size, true);
            assertNotNull("H5allocate_memory (cleared) should return non-null", mem);
            assertFalse("Allocated memory should not be NULL segment", mem.equals(MemorySegment.NULL));

            // Verify first few bytes are zero (memory was cleared)
            byte first = mem.get(ValueLayout.JAVA_BYTE, 0);
            assertEquals("Cleared memory should be zero", 0, first);

            // Free the memory
            int result = hdf5_h.H5free_memory(mem);
            assertTrue("H5free_memory should succeed", isSuccess(result));
        }
    }

    @Test
    public void testH5resize_memory()
    {
        try (Arena arena = Arena.ofConfined()) {
            // Allocate initial memory
            long initialSize  = 1024;
            MemorySegment mem = hdf5_h.H5allocate_memory(initialSize, false);
            assertNotNull("Initial allocation should succeed", mem);

            // Resize to larger size
            long newSize          = 2048;
            MemorySegment resized = hdf5_h.H5resize_memory(mem, newSize);
            assertNotNull("H5resize_memory should return non-null", resized);
            assertFalse("Resized memory should not be NULL segment", resized.equals(MemorySegment.NULL));

            // Free the resized memory
            int result = hdf5_h.H5free_memory(resized);
            assertTrue("H5free_memory should succeed", isSuccess(result));
        }
    }

    @Test
    public void testH5free_memory()
    {
        try (Arena arena = Arena.ofConfined()) {
            // Allocate and immediately free
            MemorySegment mem = hdf5_h.H5allocate_memory(512, false);
            assertNotNull("Allocation should succeed", mem);

            int result = hdf5_h.H5free_memory(mem);
            assertTrue("H5free_memory should succeed", isSuccess(result));
        }
    }

    // ================================
    // Free List Management
    // ================================

    @Test
    public void testH5set_free_list_limits()
    {
        // Set conservative limits for free lists
        int reg_global_lim = 1; // 1 MB
        int reg_list_lim   = 1; // 1 MB
        int arr_global_lim = 1; // 1 MB
        int arr_list_lim   = 1; // 1 MB
        int blk_global_lim = 1; // 1 MB
        int blk_list_lim   = 1; // 1 MB

        int result = hdf5_h.H5set_free_list_limits(reg_global_lim, reg_list_lim, arr_global_lim, arr_list_lim,
                                                   blk_global_lim, blk_list_lim);
        assertTrue("H5set_free_list_limits should succeed", isSuccess(result));
    }

    @Test
    public void testH5get_free_list_sizes()
    {
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment regSize = arena.allocate(ValueLayout.JAVA_LONG);
            MemorySegment arrSize = arena.allocate(ValueLayout.JAVA_LONG);
            MemorySegment blkSize = arena.allocate(ValueLayout.JAVA_LONG);
            MemorySegment facSize = arena.allocate(ValueLayout.JAVA_LONG);

            int result = hdf5_h.H5get_free_list_sizes(regSize, arrSize, blkSize, facSize);
            assertTrue("H5get_free_list_sizes failed", isSuccess(result));

            long reg = regSize.get(ValueLayout.JAVA_LONG, 0);
            long arr = arrSize.get(ValueLayout.JAVA_LONG, 0);
            long blk = blkSize.get(ValueLayout.JAVA_LONG, 0);
            long fac = facSize.get(ValueLayout.JAVA_LONG, 0);

            // Sizes should be non-negative
            assertTrue("Regular free list size should be >= 0", reg >= 0);
            assertTrue("Array free list size should be >= 0", arr >= 0);
            assertTrue("Block free list size should be >= 0", blk >= 0);
            assertTrue("Factory free list size should be >= 0", fac >= 0);

            System.out.print(" [reg=" + reg + ",arr=" + arr + ",blk=" + blk + ",fac=" + fac + "]");
        }
    }
}
