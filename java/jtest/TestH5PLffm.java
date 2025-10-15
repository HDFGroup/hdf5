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
 * FFM-only tests for HDF5 Plugin (H5PL) operations.
 *
 * NOTE: These tests focus on plugin path management and loading state control.
 * Actual plugin loading requires external plugin libraries and specialized setup.
 */
public class TestH5PLffm {
    @Rule
    public TestName testname = new TestName();

    // Store initial state to restore after tests
    private int initialPluginState;
    private int initialPathCount;

    @Before
    public void setUp()
    {
        System.out.print(testname.getMethodName());

        // Save initial plugin loading state
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment stateSeg = arena.allocate(ValueLayout.JAVA_INT);
            int result             = hdf5_h.H5PLget_loading_state(stateSeg);
            if (result >= 0) {
                initialPluginState = stateSeg.get(ValueLayout.JAVA_INT, 0);
            }
        }

        // Save initial path count
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment countSeg = arena.allocate(ValueLayout.JAVA_INT);
            int result             = hdf5_h.H5PLsize(countSeg);
            if (result >= 0) {
                initialPathCount = countSeg.get(ValueLayout.JAVA_INT, 0);
            }
        }
    }

    @After
    public void tearDown()
    {
        // Restore initial plugin loading state
        hdf5_h.H5PLset_loading_state(initialPluginState);

        // Remove any paths added during testing (in reverse order)
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment countSeg = arena.allocate(ValueLayout.JAVA_INT);
            hdf5_h.H5PLsize(countSeg);
            int currentCount = countSeg.get(ValueLayout.JAVA_INT, 0);

            // Remove paths added by tests
            while (currentCount > initialPathCount) {
                hdf5_h.H5PLremove(currentCount - 1);
                currentCount--;
            }
        }

        System.out.println();
    }

    /**
     * Test H5PLset_loading_state and H5PLget_loading_state
     */
    @Test
    public void testH5PLset_and_get_loading_state()
    {
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment stateSeg = arena.allocate(ValueLayout.JAVA_INT);

            // Get initial state
            int result = hdf5_h.H5PLget_loading_state(stateSeg);
            assertEquals("H5PLget_loading_state should succeed", 0, result);
            int originalState = stateSeg.get(ValueLayout.JAVA_INT, 0);

            // Enable all plugins
            result = hdf5_h.H5PLset_loading_state(hdf5_h.H5PL_ALL_PLUGIN());
            assertEquals("H5PLset_loading_state should succeed", 0, result);

            result = hdf5_h.H5PLget_loading_state(stateSeg);
            assertEquals("H5PLget_loading_state should succeed", 0, result);
            int allState = stateSeg.get(ValueLayout.JAVA_INT, 0);
            assertEquals("All plugins should be enabled", hdf5_h.H5PL_ALL_PLUGIN(), allState);

            // Enable only filter plugins
            result = hdf5_h.H5PLset_loading_state(hdf5_h.H5PL_FILTER_PLUGIN());
            assertEquals("H5PLset_loading_state should succeed", 0, result);

            result = hdf5_h.H5PLget_loading_state(stateSeg);
            assertEquals("H5PLget_loading_state should succeed", 0, result);
            int filterState = stateSeg.get(ValueLayout.JAVA_INT, 0);
            assertEquals("Only filter plugins should be enabled", hdf5_h.H5PL_FILTER_PLUGIN(), filterState);

            // Disable all plugins
            result = hdf5_h.H5PLset_loading_state(0);
            assertEquals("H5PLset_loading_state should succeed", 0, result);

            result = hdf5_h.H5PLget_loading_state(stateSeg);
            assertEquals("H5PLget_loading_state should succeed", 0, result);
            int disabledState = stateSeg.get(ValueLayout.JAVA_INT, 0);
            assertEquals("All plugins should be disabled", 0, disabledState);

            // Restore original state
            hdf5_h.H5PLset_loading_state(originalState);
        }
    }

    /**
     * Test H5PLsize - get number of plugin search paths
     */
    @Test
    public void testH5PLsize()
    {
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment countSeg = arena.allocate(ValueLayout.JAVA_INT);

            int result = hdf5_h.H5PLsize(countSeg);
            assertEquals("H5PLsize should succeed", 0, result);

            int pathCount = countSeg.get(ValueLayout.JAVA_INT, 0);
            assertTrue("Path count should be non-negative", pathCount >= 0);
        }
    }

    /**
     * Test H5PLappend - add path to end of search list
     */
    @Test
    public void testH5PLappend()
    {
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment countSeg = arena.allocate(ValueLayout.JAVA_INT);

            // Get initial count
            hdf5_h.H5PLsize(countSeg);
            int initialCount = countSeg.get(ValueLayout.JAVA_INT, 0);

            // Append a test path
            MemorySegment testPath = stringToSegment(arena, "/tmp/test_plugin_path");
            int result             = hdf5_h.H5PLappend(testPath);
            assertEquals("H5PLappend should succeed", 0, result);

            // Verify count increased
            hdf5_h.H5PLsize(countSeg);
            int newCount = countSeg.get(ValueLayout.JAVA_INT, 0);
            assertEquals("Path count should increase by 1", initialCount + 1, newCount);

            // Verify the path was added at the end
            long size = hdf5_h.H5PLget(newCount - 1, MemorySegment.NULL, 0);
            assertTrue("Should get path size", size > 0);

            MemorySegment pathBuf = arena.allocate(ValueLayout.JAVA_BYTE, (int)size + 1);
            size                  = hdf5_h.H5PLget(newCount - 1, pathBuf, size + 1);
            String retrievedPath  = segmentToString(pathBuf);
            assertEquals("Retrieved path should match", "/tmp/test_plugin_path", retrievedPath);
        }
    }

    /**
     * Test H5PLprepend - add path to beginning of search list
     */
    @Test
    public void testH5PLprepend()
    {
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment countSeg = arena.allocate(ValueLayout.JAVA_INT);

            // Get initial count
            hdf5_h.H5PLsize(countSeg);
            int initialCount = countSeg.get(ValueLayout.JAVA_INT, 0);

            // Prepend a test path
            MemorySegment testPath = stringToSegment(arena, "/tmp/test_prepend_path");
            int result             = hdf5_h.H5PLprepend(testPath);
            assertEquals("H5PLprepend should succeed", 0, result);

            // Verify count increased
            hdf5_h.H5PLsize(countSeg);
            int newCount = countSeg.get(ValueLayout.JAVA_INT, 0);
            assertEquals("Path count should increase by 1", initialCount + 1, newCount);

            // Verify the path was added at the beginning (index 0)
            long size = hdf5_h.H5PLget(0, MemorySegment.NULL, 0);
            assertTrue("Should get path size", size > 0);

            MemorySegment pathBuf = arena.allocate(ValueLayout.JAVA_BYTE, (int)size + 1);
            size                  = hdf5_h.H5PLget(0, pathBuf, size + 1);
            String retrievedPath  = segmentToString(pathBuf);
            assertEquals("Retrieved path should match", "/tmp/test_prepend_path", retrievedPath);
        }
    }

    /**
     * Test H5PLinsert - insert path at specific index
     */
    @Test
    public void testH5PLinsert()
    {
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment countSeg = arena.allocate(ValueLayout.JAVA_INT);

            // Add two paths first
            MemorySegment path1 = stringToSegment(arena, "/tmp/path1");
            MemorySegment path2 = stringToSegment(arena, "/tmp/path2");
            hdf5_h.H5PLappend(path1);
            hdf5_h.H5PLappend(path2);

            // Get count before insert
            hdf5_h.H5PLsize(countSeg);
            int beforeCount = countSeg.get(ValueLayout.JAVA_INT, 0);

            // Insert path at index 1 (between path1 and path2)
            MemorySegment insertPath = stringToSegment(arena, "/tmp/path_inserted");
            int result               = hdf5_h.H5PLinsert(insertPath, beforeCount - 1);
            assertEquals("H5PLinsert should succeed", 0, result);

            // Verify count increased
            hdf5_h.H5PLsize(countSeg);
            int afterCount = countSeg.get(ValueLayout.JAVA_INT, 0);
            assertEquals("Path count should increase by 1", beforeCount + 1, afterCount);

            // Verify the path was inserted at correct position
            long size             = hdf5_h.H5PLget(beforeCount - 1, MemorySegment.NULL, 0);
            MemorySegment pathBuf = arena.allocate(ValueLayout.JAVA_BYTE, (int)size + 1);
            hdf5_h.H5PLget(beforeCount - 1, pathBuf, size + 1);
            String retrievedPath = segmentToString(pathBuf);
            assertEquals("Retrieved path should match", "/tmp/path_inserted", retrievedPath);
        }
    }

    /**
     * Test H5PLreplace - replace path at specific index
     */
    @Test
    public void testH5PLreplace()
    {
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment countSeg = arena.allocate(ValueLayout.JAVA_INT);

            // Add a test path
            MemorySegment originalPath = stringToSegment(arena, "/tmp/original_path");
            hdf5_h.H5PLappend(originalPath);

            // Get count and index of last path
            hdf5_h.H5PLsize(countSeg);
            int count   = countSeg.get(ValueLayout.JAVA_INT, 0);
            int lastIdx = count - 1;

            // Replace the last path
            MemorySegment replacePath = stringToSegment(arena, "/tmp/replacement_path");
            int result                = hdf5_h.H5PLreplace(replacePath, lastIdx);
            assertEquals("H5PLreplace should succeed", 0, result);

            // Verify count unchanged
            hdf5_h.H5PLsize(countSeg);
            int newCount = countSeg.get(ValueLayout.JAVA_INT, 0);
            assertEquals("Path count should remain the same", count, newCount);

            // Verify the path was replaced
            long size             = hdf5_h.H5PLget(lastIdx, MemorySegment.NULL, 0);
            MemorySegment pathBuf = arena.allocate(ValueLayout.JAVA_BYTE, (int)size + 1);
            hdf5_h.H5PLget(lastIdx, pathBuf, size + 1);
            String retrievedPath = segmentToString(pathBuf);
            assertEquals("Retrieved path should be replacement", "/tmp/replacement_path", retrievedPath);
        }
    }

    /**
     * Test H5PLremove - remove path at specific index
     */
    @Test
    public void testH5PLremove()
    {
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment countSeg = arena.allocate(ValueLayout.JAVA_INT);

            // Add a test path
            MemorySegment testPath = stringToSegment(arena, "/tmp/path_to_remove");
            hdf5_h.H5PLappend(testPath);

            // Get count before removal
            hdf5_h.H5PLsize(countSeg);
            int beforeCount = countSeg.get(ValueLayout.JAVA_INT, 0);

            // Remove the last path
            int result = hdf5_h.H5PLremove(beforeCount - 1);
            assertEquals("H5PLremove should succeed", 0, result);

            // Verify count decreased
            hdf5_h.H5PLsize(countSeg);
            int afterCount = countSeg.get(ValueLayout.JAVA_INT, 0);
            assertEquals("Path count should decrease by 1", beforeCount - 1, afterCount);
        }
    }

    /**
     * Test H5PLget - retrieve path at specific index
     */
    @Test
    public void testH5PLget()
    {
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment countSeg = arena.allocate(ValueLayout.JAVA_INT);

            // Add a known test path
            String knownPath       = "/tmp/test_get_path";
            MemorySegment testPath = stringToSegment(arena, knownPath);
            hdf5_h.H5PLappend(testPath);

            // Get the index of the path we just added
            hdf5_h.H5PLsize(countSeg);
            int count   = countSeg.get(ValueLayout.JAVA_INT, 0);
            int lastIdx = count - 1;

            // First call to get size (with NULL buffer)
            long size = hdf5_h.H5PLget(lastIdx, MemorySegment.NULL, 0);
            assertTrue("Should return path length", size > 0);

            // Second call to get actual path
            MemorySegment pathBuf = arena.allocate(ValueLayout.JAVA_BYTE, (int)size + 1);
            long actualSize       = hdf5_h.H5PLget(lastIdx, pathBuf, size + 1);
            assertEquals("Sizes should match", size, actualSize);

            String retrievedPath = segmentToString(pathBuf);
            assertEquals("Retrieved path should match", knownPath, retrievedPath);
        }
    }

    /**
     * Test H5PLget with invalid index
     */
    @Test
    public void testH5PLget_invalid_index()
    {
        try (Arena arena = Arena.ofConfined()) {
            // Try to get path at very large invalid index
            long size = hdf5_h.H5PLget(99999, MemorySegment.NULL, 0);

            // Should return negative value or zero for invalid index
            assertTrue("Should return error for invalid index", size <= 0);
        }
    }

    /**
     * Test plugin type constants
     */
    @Test
    public void testH5PL_plugin_type_constants()
    {
        // Verify plugin type flag constants exist and have expected values
        int filterPlugin = hdf5_h.H5PL_FILTER_PLUGIN();
        int volPlugin    = hdf5_h.H5PL_VOL_PLUGIN();
        int vfdPlugin    = hdf5_h.H5PL_VFD_PLUGIN();
        int allPlugin    = hdf5_h.H5PL_ALL_PLUGIN();

        // Filter plugin should be bit 0
        assertEquals("H5PL_FILTER_PLUGIN should be 0x0001", 0x0001, filterPlugin);

        // VOL plugin should be bit 1
        assertEquals("H5PL_VOL_PLUGIN should be 0x0002", 0x0002, volPlugin);

        // VFD plugin should be bit 2
        assertEquals("H5PL_VFD_PLUGIN should be 0x0004", 0x0004, vfdPlugin);

        // All plugins should be 0xFFFF
        assertEquals("H5PL_ALL_PLUGIN should be 0xFFFF", 0xFFFF, allPlugin);
    }

    /**
     * Test multiple plugin types enabled simultaneously
     */
    @Test
    public void testH5PL_multiple_plugin_types()
    {
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment stateSeg = arena.allocate(ValueLayout.JAVA_INT);

            // Enable filter and VOL plugins
            int combinedMask = hdf5_h.H5PL_FILTER_PLUGIN() | hdf5_h.H5PL_VOL_PLUGIN();
            int result       = hdf5_h.H5PLset_loading_state(combinedMask);
            assertEquals("H5PLset_loading_state should succeed", 0, result);

            // Verify state
            hdf5_h.H5PLget_loading_state(stateSeg);
            int state = stateSeg.get(ValueLayout.JAVA_INT, 0);
            assertEquals("State should match combined mask", combinedMask, state);

            // Verify individual bits are set
            assertTrue("Filter plugin should be enabled", (state & hdf5_h.H5PL_FILTER_PLUGIN()) != 0);
            assertTrue("VOL plugin should be enabled", (state & hdf5_h.H5PL_VOL_PLUGIN()) != 0);
            assertFalse("VFD plugin should not be enabled", (state & hdf5_h.H5PL_VFD_PLUGIN()) != 0);
        }
    }
}
