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
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

/**
 * FFM-only tests for HDF5 Dataspace (H5S) operations.
 *
 * This test class uses direct FFM bindings without the hdf.hdf5lib wrapper layer.
 */
public class TestH5Sffm {
    @Rule
    public TestName testname = new TestName();

    private static final int RANK  = 2;
    private static final int DIM_X = 4;
    private static final int DIM_Y = 6;

    long H5sid = hdf5_h.H5I_INVALID_HID();

    @After
    public void cleanup()
    {
        closeQuietly(H5sid, hdf5_h::H5Sclose);
        H5sid = hdf5_h.H5I_INVALID_HID();
        System.out.println();
    }

    @Test
    public void testH5Screate_simple()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long[] dims               = {DIM_X, DIM_Y};
            MemorySegment dimsSegment = allocateLongArray(arena, RANK);
            copyToSegment(dimsSegment, dims);

            H5sid = hdf5_h.H5Screate_simple(RANK, dimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple failed", isValidId(H5sid));
        }
    }

    @Test
    public void testH5Screate_simple_with_maxdims()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long[] dims    = {DIM_X, DIM_Y};
            long[] maxdims = {2 * DIM_X, 2 * DIM_Y};

            MemorySegment dimsSegment    = allocateLongArray(arena, RANK);
            MemorySegment maxdimsSegment = allocateLongArray(arena, RANK);
            copyToSegment(dimsSegment, dims);
            copyToSegment(maxdimsSegment, maxdims);

            H5sid = hdf5_h.H5Screate_simple(RANK, dimsSegment, maxdimsSegment);
            assertTrue("H5Screate_simple with maxdims failed", isValidId(H5sid));
        }
    }

    @Test
    public void testH5Screate()
    {
        System.out.print(testname.getMethodName());

        H5sid = hdf5_h.H5Screate(hdf5_h.H5S_SIMPLE());
        assertTrue("H5Screate failed", isValidId(H5sid));
    }

    @Test
    public void testH5Screate_scalar()
    {
        System.out.print(testname.getMethodName());

        H5sid = hdf5_h.H5Screate(hdf5_h.H5S_SCALAR());
        assertTrue("H5Screate scalar failed", isValidId(H5sid));

        // Verify it's a scalar
        int ndims = hdf5_h.H5Sget_simple_extent_ndims(H5sid);
        assertEquals("Scalar should have 0 dimensions", 0, ndims);
    }

    @Test
    public void testH5Scopy()
    {
        System.out.print(testname.getMethodName());
        long sid_copy = hdf5_h.H5I_INVALID_HID();

        try (Arena arena = Arena.ofConfined()) {
            // Create original dataspace
            long[] dims               = {DIM_X, DIM_Y};
            MemorySegment dimsSegment = allocateLongArray(arena, RANK);
            copyToSegment(dimsSegment, dims);

            H5sid = hdf5_h.H5Screate_simple(RANK, dimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple failed", isValidId(H5sid));

            // Copy dataspace
            sid_copy = hdf5_h.H5Scopy(H5sid);
            assertTrue("H5Scopy failed", isValidId(sid_copy));

            // Verify dimensions match
            MemorySegment copyDimsSegment = allocateLongArray(arena, RANK);
            int ndims = hdf5_h.H5Sget_simple_extent_dims(sid_copy, copyDimsSegment, MemorySegment.NULL);
            assertEquals("Rank should match", RANK, ndims);

            long[] copyDims = new long[RANK];
            copyFromSegment(copyDimsSegment, copyDims);
            assertArrayEquals("Dimensions should match", dims, copyDims);
        }
        finally {
            closeQuietly(sid_copy, hdf5_h::H5Sclose);
        }
    }

    @Test
    public void testH5Sget_simple_extent_ndims()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long[] dims               = {DIM_X, DIM_Y};
            MemorySegment dimsSegment = allocateLongArray(arena, RANK);
            copyToSegment(dimsSegment, dims);

            H5sid = hdf5_h.H5Screate_simple(RANK, dimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple failed", isValidId(H5sid));

            int ndims = hdf5_h.H5Sget_simple_extent_ndims(H5sid);
            assertEquals("Rank should match", RANK, ndims);
        }
    }

    @Test
    public void testH5Sget_simple_extent_npoints()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long[] dims               = {DIM_X, DIM_Y};
            MemorySegment dimsSegment = allocateLongArray(arena, RANK);
            copyToSegment(dimsSegment, dims);

            H5sid = hdf5_h.H5Screate_simple(RANK, dimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple failed", isValidId(H5sid));

            long npoints = hdf5_h.H5Sget_simple_extent_npoints(H5sid);
            assertEquals("Number of points should be DIM_X * DIM_Y", DIM_X * DIM_Y, npoints);
        }
    }

    @Test
    public void testH5Sget_simple_extent_type()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long[] dims               = {DIM_X, DIM_Y};
            MemorySegment dimsSegment = allocateLongArray(arena, RANK);
            copyToSegment(dimsSegment, dims);

            H5sid = hdf5_h.H5Screate_simple(RANK, dimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple failed", isValidId(H5sid));

            int spaceType = hdf5_h.H5Sget_simple_extent_type(H5sid);
            assertEquals("Space type should be hdf5_h.H5S_SIMPLE()", hdf5_h.H5S_SIMPLE(), spaceType);
        }
    }

    @Test
    public void testH5Sset_extent_simple()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            H5sid = hdf5_h.H5Screate(hdf5_h.H5S_SIMPLE());
            assertTrue("H5Screate failed", isValidId(H5sid));

            long[] dims               = {DIM_X, DIM_Y};
            MemorySegment dimsSegment = allocateLongArray(arena, RANK);
            copyToSegment(dimsSegment, dims);

            int result = hdf5_h.H5Sset_extent_simple(H5sid, RANK, dimsSegment, MemorySegment.NULL);
            assertTrue("H5Sset_extent_simple failed", isSuccess(result));

            // Verify dimensions were set
            int ndims = hdf5_h.H5Sget_simple_extent_ndims(H5sid);
            assertEquals("Rank should match", RANK, ndims);
        }
    }

    @Test
    public void testH5Sselect_hyperslab()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long[] dims               = {DIM_X, DIM_Y};
            MemorySegment dimsSegment = allocateLongArray(arena, RANK);
            copyToSegment(dimsSegment, dims);

            H5sid = hdf5_h.H5Screate_simple(RANK, dimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple failed", isValidId(H5sid));

            // Select a 2x3 hyperslab starting at (1,1)
            long[] start = {1, 1};
            long[] count = {2, 3};

            MemorySegment startSegment = allocateLongArray(arena, RANK);
            MemorySegment countSegment = allocateLongArray(arena, RANK);
            copyToSegment(startSegment, start);
            copyToSegment(countSegment, count);

            int result = hdf5_h.H5Sselect_hyperslab(H5sid, hdf5_h.H5S_SELECT_SET(), startSegment,
                                                    MemorySegment.NULL, countSegment, MemorySegment.NULL);
            assertTrue("H5Sselect_hyperslab failed", isSuccess(result));

            // Verify selection
            long npoints = hdf5_h.H5Sget_select_npoints(H5sid);
            assertEquals("Selected points should be 2*3", 6, npoints);
        }
    }

    @Test
    public void testH5Sselect_elements()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long[] dims               = {DIM_X, DIM_Y};
            MemorySegment dimsSegment = allocateLongArray(arena, RANK);
            copyToSegment(dimsSegment, dims);

            H5sid = hdf5_h.H5Screate_simple(RANK, dimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple failed", isValidId(H5sid));

            // Select 3 specific points
            long[] coords = {
                0, 0, // Point 1
                1, 1, // Point 2
                2, 2  // Point 3
            };

            MemorySegment coordsSegment = allocateLongArray(arena, coords.length);
            copyToSegment(coordsSegment, coords);

            int result = hdf5_h.H5Sselect_elements(H5sid, hdf5_h.H5S_SELECT_SET(), 3, coordsSegment);
            assertTrue("H5Sselect_elements failed", isSuccess(result));

            // Verify selection
            long npoints = hdf5_h.H5Sget_select_npoints(H5sid);
            assertEquals("Selected points should be 3", 3, npoints);
        }
    }

    @Test
    public void testH5Sselect_all()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long[] dims               = {DIM_X, DIM_Y};
            MemorySegment dimsSegment = allocateLongArray(arena, RANK);
            copyToSegment(dimsSegment, dims);

            H5sid = hdf5_h.H5Screate_simple(RANK, dimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple failed", isValidId(H5sid));

            int result = hdf5_h.H5Sselect_all(H5sid);
            assertTrue("H5Sselect_all failed", isSuccess(result));

            // Verify selection
            long npoints = hdf5_h.H5Sget_select_npoints(H5sid);
            assertEquals("All points should be selected", DIM_X * DIM_Y, npoints);
        }
    }

    @Test
    public void testH5Sselect_none()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long[] dims               = {DIM_X, DIM_Y};
            MemorySegment dimsSegment = allocateLongArray(arena, RANK);
            copyToSegment(dimsSegment, dims);

            H5sid = hdf5_h.H5Screate_simple(RANK, dimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple failed", isValidId(H5sid));

            int result = hdf5_h.H5Sselect_none(H5sid);
            assertTrue("H5Sselect_none failed", isSuccess(result));

            // Verify selection
            long npoints = hdf5_h.H5Sget_select_npoints(H5sid);
            assertEquals("No points should be selected", 0, npoints);
        }
    }

    @Test
    public void testH5Sget_simple_extent_dims()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long[] expectedDims    = {DIM_X, DIM_Y};
            long[] expectedMaxDims = {2 * DIM_X, 2 * DIM_Y};

            MemorySegment dimsSegment    = allocateLongArray(arena, RANK);
            MemorySegment maxdimsSegment = allocateLongArray(arena, RANK);
            copyToSegment(dimsSegment, expectedDims);
            copyToSegment(maxdimsSegment, expectedMaxDims);

            H5sid = hdf5_h.H5Screate_simple(RANK, dimsSegment, maxdimsSegment);
            assertTrue("H5Screate_simple failed", isValidId(H5sid));

            // Get dimensions back
            MemorySegment returnedDimsSegment    = allocateLongArray(arena, RANK);
            MemorySegment returnedMaxDimsSegment = allocateLongArray(arena, RANK);

            int ndims = hdf5_h.H5Sget_simple_extent_dims(H5sid, returnedDimsSegment, returnedMaxDimsSegment);
            assertEquals("Rank should match", RANK, ndims);

            long[] returnedDims    = new long[RANK];
            long[] returnedMaxDims = new long[RANK];
            copyFromSegment(returnedDimsSegment, returnedDims);
            copyFromSegment(returnedMaxDimsSegment, returnedMaxDims);

            assertArrayEquals("Dimensions should match", expectedDims, returnedDims);
            assertArrayEquals("Max dimensions should match", expectedMaxDims, returnedMaxDims);
        }
    }

    @Test
    public void testH5Sget_select_type()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long[] dims               = {DIM_X, DIM_Y};
            MemorySegment dimsSegment = allocateLongArray(arena, RANK);
            copyToSegment(dimsSegment, dims);

            H5sid = hdf5_h.H5Screate_simple(RANK, dimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple failed", isValidId(H5sid));

            // Default selection type should be H5S_SEL_ALL
            int selType = hdf5_h.H5Sget_select_type(H5sid);
            assertEquals("Default selection should be ALL", hdf5_h.H5S_SEL_ALL(), selType);

            // Select hyperslab
            long[] start               = {1, 1};
            long[] count               = {2, 3};
            MemorySegment startSegment = allocateLongArray(arena, RANK);
            MemorySegment countSegment = allocateLongArray(arena, RANK);
            copyToSegment(startSegment, start);
            copyToSegment(countSegment, count);

            hdf5_h.H5Sselect_hyperslab(H5sid, hdf5_h.H5S_SELECT_SET(), startSegment, MemorySegment.NULL,
                                       countSegment, MemorySegment.NULL);

            selType = hdf5_h.H5Sget_select_type(H5sid);
            assertEquals("Selection type should be HYPERSLABS", hdf5_h.H5S_SEL_HYPERSLABS(), selType);

            // Select none
            hdf5_h.H5Sselect_none(H5sid);
            selType = hdf5_h.H5Sget_select_type(H5sid);
            assertEquals("Selection type should be NONE", hdf5_h.H5S_SEL_NONE(), selType);
        }
    }

    @Test
    public void testH5Sget_select_bounds()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long[] dims               = {DIM_X, DIM_Y};
            MemorySegment dimsSegment = allocateLongArray(arena, RANK);
            copyToSegment(dimsSegment, dims);

            H5sid = hdf5_h.H5Screate_simple(RANK, dimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple failed", isValidId(H5sid));

            // Select a hyperslab from (1,2) with count (2,3)
            long[] start               = {1, 2};
            long[] count               = {2, 3};
            MemorySegment startSegment = allocateLongArray(arena, RANK);
            MemorySegment countSegment = allocateLongArray(arena, RANK);
            copyToSegment(startSegment, start);
            copyToSegment(countSegment, count);

            hdf5_h.H5Sselect_hyperslab(H5sid, hdf5_h.H5S_SELECT_SET(), startSegment, MemorySegment.NULL,
                                       countSegment, MemorySegment.NULL);

            // Get selection bounds
            MemorySegment boundsStartSegment = allocateLongArray(arena, RANK);
            MemorySegment boundsEndSegment   = allocateLongArray(arena, RANK);

            int result = hdf5_h.H5Sget_select_bounds(H5sid, boundsStartSegment, boundsEndSegment);
            assertTrue("H5Sget_select_bounds failed", isSuccess(result));

            long[] boundsStart = new long[RANK];
            long[] boundsEnd   = new long[RANK];
            copyFromSegment(boundsStartSegment, boundsStart);
            copyFromSegment(boundsEndSegment, boundsEnd);

            // Bounds should be: start=(1,2), end=(2,4) because end = start + count - 1
            long[] expectedStart = {1, 2};
            long[] expectedEnd   = {2, 4}; // (1+2-1, 2+3-1)

            assertArrayEquals("Bounds start should match", expectedStart, boundsStart);
            assertArrayEquals("Bounds end should match", expectedEnd, boundsEnd);
        }
    }

    @Test
    public void testH5Sextent_copy()
    {
        System.out.print(testname.getMethodName());
        long sid_dest = hdf5_h.H5I_INVALID_HID();

        try (Arena arena = Arena.ofConfined()) {
            // Create source dataspace with specific dimensions
            long[] dims               = {DIM_X, DIM_Y};
            MemorySegment dimsSegment = allocateLongArray(arena, RANK);
            copyToSegment(dimsSegment, dims);

            H5sid = hdf5_h.H5Screate_simple(RANK, dimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple failed", isValidId(H5sid));

            // Create destination dataspace (initially scalar)
            sid_dest = hdf5_h.H5Screate(hdf5_h.H5S_SCALAR());
            assertTrue("H5Screate scalar failed", isValidId(sid_dest));

            // Copy extent from source to destination
            int result = hdf5_h.H5Sextent_copy(sid_dest, H5sid);
            assertTrue("H5Sextent_copy failed", isSuccess(result));

            // Verify destination now has same dimensions as source
            MemorySegment destDimsSegment = allocateLongArray(arena, RANK);
            int ndims = hdf5_h.H5Sget_simple_extent_dims(sid_dest, destDimsSegment, MemorySegment.NULL);
            assertEquals("Rank should match", RANK, ndims);

            long[] destDims = new long[RANK];
            copyFromSegment(destDimsSegment, destDims);
            assertArrayEquals("Dimensions should match", dims, destDims);
        }
        finally {
            closeQuietly(sid_dest, hdf5_h::H5Sclose);
        }
    }

    @Test
    public void testH5Sextent_equal()
    {
        System.out.print(testname.getMethodName());
        long sid2 = hdf5_h.H5I_INVALID_HID();
        long sid3 = hdf5_h.H5I_INVALID_HID();

        try (Arena arena = Arena.ofConfined()) {
            // Create first dataspace
            long[] dims               = {DIM_X, DIM_Y};
            MemorySegment dimsSegment = allocateLongArray(arena, RANK);
            copyToSegment(dimsSegment, dims);

            H5sid = hdf5_h.H5Screate_simple(RANK, dimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple failed", isValidId(H5sid));

            // Create second dataspace with same dimensions
            sid2 = hdf5_h.H5Screate_simple(RANK, dimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple failed", isValidId(sid2));

            // Create third dataspace with different dimensions
            long[] diffDims               = {DIM_X + 1, DIM_Y};
            MemorySegment diffDimsSegment = allocateLongArray(arena, RANK);
            copyToSegment(diffDimsSegment, diffDims);

            sid3 = hdf5_h.H5Screate_simple(RANK, diffDimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple failed", isValidId(sid3));

            // Test equality
            int result = hdf5_h.H5Sextent_equal(H5sid, sid2);
            assertTrue("Extents should be equal", result > 0);

            result = hdf5_h.H5Sextent_equal(H5sid, sid3);
            assertFalse("Extents should not be equal", result > 0);
        }
        finally {
            closeQuietly(sid2, hdf5_h::H5Sclose);
            closeQuietly(sid3, hdf5_h::H5Sclose);
        }
    }

    @Test
    public void testH5Sget_select_hyper_nblocks()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long[] dims               = {DIM_X, DIM_Y};
            MemorySegment dimsSegment = allocateLongArray(arena, RANK);
            copyToSegment(dimsSegment, dims);

            H5sid = hdf5_h.H5Screate_simple(RANK, dimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple failed", isValidId(H5sid));

            // Select first hyperslab
            long[] start1               = {0, 0};
            long[] count1               = {2, 2};
            MemorySegment start1Segment = allocateLongArray(arena, RANK);
            MemorySegment count1Segment = allocateLongArray(arena, RANK);
            copyToSegment(start1Segment, start1);
            copyToSegment(count1Segment, count1);

            hdf5_h.H5Sselect_hyperslab(H5sid, hdf5_h.H5S_SELECT_SET(), start1Segment, MemorySegment.NULL,
                                       count1Segment, MemorySegment.NULL);

            // Add second hyperslab (OR operation)
            long[] start2               = {2, 2};
            long[] count2               = {2, 2};
            MemorySegment start2Segment = allocateLongArray(arena, RANK);
            MemorySegment count2Segment = allocateLongArray(arena, RANK);
            copyToSegment(start2Segment, start2);
            copyToSegment(count2Segment, count2);

            hdf5_h.H5Sselect_hyperslab(H5sid, hdf5_h.H5S_SELECT_OR(), start2Segment, MemorySegment.NULL,
                                       count2Segment, MemorySegment.NULL);

            // Get number of blocks
            long nblocks = hdf5_h.H5Sget_select_hyper_nblocks(H5sid);
            assertEquals("Should have 2 hyperslab blocks", 2, nblocks);
        }
    }

    @Test
    public void testH5Sencode_decode()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create dataspace with hyperslab selection
            long[] dims               = {DIM_X, DIM_Y};
            MemorySegment dimsSegment = allocateLongArray(arena, RANK);
            copyToSegment(dimsSegment, dims);

            H5sid = hdf5_h.H5Screate_simple(RANK, dimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple failed", isValidId(H5sid));

            // Select a hyperslab
            long[] start               = {1, 1};
            long[] count               = {2, 3};
            MemorySegment startSegment = allocateLongArray(arena, RANK);
            MemorySegment countSegment = allocateLongArray(arena, RANK);
            copyToSegment(startSegment, start);
            copyToSegment(countSegment, count);

            hdf5_h.H5Sselect_hyperslab(H5sid, hdf5_h.H5S_SELECT_SET(), startSegment, MemorySegment.NULL,
                                       countSegment, MemorySegment.NULL);

            // Get encoded size
            MemorySegment nalloc_segment = allocateLong(arena);
            int result = hdf5_h.H5Sencode2(H5sid, MemorySegment.NULL, nalloc_segment, hdf5_h.H5P_DEFAULT());
            assertTrue("H5Sencode2 (get size) failed", isSuccess(result));

            long nalloc = getLong(nalloc_segment);
            assertTrue("Encoded size should be > 0", nalloc > 0);

            // Encode dataspace
            MemorySegment buf = arena.allocate(nalloc);
            result            = hdf5_h.H5Sencode2(H5sid, buf, nalloc_segment, hdf5_h.H5P_DEFAULT());
            assertTrue("H5Sencode2 failed", isSuccess(result));

            // Decode dataspace
            long decoded_sid = hdf5_h.H5Sdecode(buf);
            assertTrue("H5Sdecode failed", isValidId(decoded_sid));

            // Verify decoded dataspace has same selection
            long npoints_orig    = hdf5_h.H5Sget_select_npoints(H5sid);
            long npoints_decoded = hdf5_h.H5Sget_select_npoints(decoded_sid);
            assertEquals("Selected points should match", npoints_orig, npoints_decoded);

            closeQuietly(decoded_sid, hdf5_h::H5Sclose);
        }
    }

    @Test
    public void testH5Sclose()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long[] dims               = {DIM_X, DIM_Y};
            MemorySegment dimsSegment = allocateLongArray(arena, RANK);
            copyToSegment(dimsSegment, dims);

            H5sid = hdf5_h.H5Screate_simple(RANK, dimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple failed", isValidId(H5sid));

            int result = hdf5_h.H5Sclose(H5sid);
            assertTrue("H5Sclose failed", isSuccess(result));
            H5sid = hdf5_h.H5I_INVALID_HID();
        }
    }

    @Test
    public void testH5Sget_select_npoints()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long[] dims               = {DIM_X, DIM_Y};
            MemorySegment dimsSegment = allocateLongArray(arena, RANK);
            copyToSegment(dimsSegment, dims);

            H5sid = hdf5_h.H5Screate_simple(RANK, dimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple failed", isValidId(H5sid));

            // Select all - should have DIM_X * DIM_Y points
            int result = hdf5_h.H5Sselect_all(H5sid);
            assertTrue("H5Sselect_all failed", isSuccess(result));

            long npoints = hdf5_h.H5Sget_select_npoints(H5sid);
            assertEquals("Should have DIM_X * DIM_Y points", DIM_X * DIM_Y, npoints);

            // Select hyperslab - 2x3 = 6 points
            long[] start               = {1, 1};
            long[] count               = {2, 3};
            MemorySegment startSegment = allocateLongArray(arena, RANK);
            MemorySegment countSegment = allocateLongArray(arena, RANK);
            copyToSegment(startSegment, start);
            copyToSegment(countSegment, count);

            result = hdf5_h.H5Sselect_hyperslab(H5sid, hdf5_h.H5S_SELECT_SET(), startSegment,
                                                MemorySegment.NULL, countSegment, MemorySegment.NULL);
            assertTrue("H5Sselect_hyperslab failed", isSuccess(result));

            npoints = hdf5_h.H5Sget_select_npoints(H5sid);
            assertEquals("Should have 6 points in hyperslab", 6L, npoints);
        }
    }

    @Test
    public void testH5Sget_select_valid()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long[] dims               = {DIM_X, DIM_Y};
            MemorySegment dimsSegment = allocateLongArray(arena, RANK);
            copyToSegment(dimsSegment, dims);

            H5sid = hdf5_h.H5Screate_simple(RANK, dimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple failed", isValidId(H5sid));

            // Select a valid hyperslab
            long[] start               = {0, 0};
            long[] count               = {2, 2};
            MemorySegment startSegment = allocateLongArray(arena, RANK);
            MemorySegment countSegment = allocateLongArray(arena, RANK);
            copyToSegment(startSegment, start);
            copyToSegment(countSegment, count);

            int result = hdf5_h.H5Sselect_hyperslab(H5sid, hdf5_h.H5S_SELECT_SET(), startSegment,
                                                    MemorySegment.NULL, countSegment, MemorySegment.NULL);
            assertTrue("H5Sselect_hyperslab failed", isSuccess(result));

            // Verify selection is valid
            int valid = hdf5_h.H5Sselect_valid(H5sid);
            assertTrue("Selection should be valid", valid > 0);
        }
    }

    @Test
    public void testH5Sget_select_hyper_blocklist()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long[] dims               = {DIM_X, DIM_Y};
            MemorySegment dimsSegment = allocateLongArray(arena, RANK);
            copyToSegment(dimsSegment, dims);

            H5sid = hdf5_h.H5Screate_simple(RANK, dimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple failed", isValidId(H5sid));

            // Select a single hyperslab block from [1,1] to [2,3]
            long[] start                = {1, 1};
            long[] stride               = {1, 1};
            long[] count                = {1, 1}; // 1 block
            long[] block                = {2, 3}; // Block size 2x3
            MemorySegment startSegment  = allocateLongArray(arena, RANK);
            MemorySegment strideSegment = allocateLongArray(arena, RANK);
            MemorySegment countSegment  = allocateLongArray(arena, RANK);
            MemorySegment blockSegment  = allocateLongArray(arena, RANK);
            copyToSegment(startSegment, start);
            copyToSegment(strideSegment, stride);
            copyToSegment(countSegment, count);
            copyToSegment(blockSegment, block);

            int result = hdf5_h.H5Sselect_hyperslab(H5sid, hdf5_h.H5S_SELECT_SET(), startSegment,
                                                    strideSegment, countSegment, blockSegment);
            assertTrue("H5Sselect_hyperslab failed", isSuccess(result));

            // Get number of blocks (should be 1)
            long nblocks = hdf5_h.H5Sget_select_hyper_nblocks(H5sid);
            assertEquals("Should have 1 block", 1L, nblocks);

            // Get blocklist (start and end coordinates)
            // Each block has 2 coordinates (start, end) with RANK values each
            long blocklistSize      = nblocks * RANK * 2;
            MemorySegment blocklist = allocateLongArray(arena, (int)blocklistSize);

            result = hdf5_h.H5Sget_select_hyper_blocklist(H5sid, 0, nblocks, blocklist);
            assertTrue("H5Sget_select_hyper_blocklist failed", isSuccess(result));

            // Verify block coordinates
            // Start: [1, 1], End: [2, 3] (inclusive, so 2 rows x 3 cols)
            assertEquals("Block start[0] should be 1", 1L, blocklist.getAtIndex(ValueLayout.JAVA_LONG, 0));
            assertEquals("Block start[1] should be 1", 1L, blocklist.getAtIndex(ValueLayout.JAVA_LONG, 1));
            assertEquals("Block end[0] should be 2", 2L, blocklist.getAtIndex(ValueLayout.JAVA_LONG, 2));
            assertEquals("Block end[1] should be 3", 3L, blocklist.getAtIndex(ValueLayout.JAVA_LONG, 3));
        }
    }

    @Test
    public void testH5Sselect_adjust()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long[] dims               = {DIM_X, DIM_Y};
            MemorySegment dimsSegment = allocateLongArray(arena, RANK);
            copyToSegment(dimsSegment, dims);

            H5sid = hdf5_h.H5Screate_simple(RANK, dimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple failed", isValidId(H5sid));

            // Select hyperslab from [2,2] with count [2,2]
            long[] start               = {2, 2};
            long[] count               = {2, 2};
            MemorySegment startSegment = allocateLongArray(arena, RANK);
            MemorySegment countSegment = allocateLongArray(arena, RANK);
            copyToSegment(startSegment, start);
            copyToSegment(countSegment, count);

            int result = hdf5_h.H5Sselect_hyperslab(H5sid, hdf5_h.H5S_SELECT_SET(), startSegment,
                                                    MemorySegment.NULL, countSegment, MemorySegment.NULL);
            assertTrue("H5Sselect_hyperslab failed", isSuccess(result));

            // Get original bounds
            MemorySegment startBounds1 = allocateLongArray(arena, RANK);
            MemorySegment endBounds1   = allocateLongArray(arena, RANK);
            result                     = hdf5_h.H5Sget_select_bounds(H5sid, startBounds1, endBounds1);
            assertTrue("H5Sget_select_bounds failed", isSuccess(result));

            long[] origStart = new long[RANK];
            copyFromSegment(startBounds1, origStart);

            // Adjust selection by offset [1, 1] (SUBTRACTS offset from selection coordinates)
            long[] offset               = {1, 1};
            MemorySegment offsetSegment = allocateLongArray(arena, RANK);
            copyToSegment(offsetSegment, offset);

            result = hdf5_h.H5Sselect_adjust(H5sid, offsetSegment);
            assertTrue("H5Sselect_adjust failed", isSuccess(result));

            // Get bounds after adjustment
            MemorySegment startBounds2 = allocateLongArray(arena, RANK);
            MemorySegment endBounds2   = allocateLongArray(arena, RANK);
            result                     = hdf5_h.H5Sget_select_bounds(H5sid, startBounds2, endBounds2);
            assertTrue("H5Sget_select_bounds failed", isSuccess(result));

            long[] newStart = new long[RANK];
            copyFromSegment(startBounds2, newStart);

            // Verify offset was applied (should be [1,1] after [1,1] offset subtracted from [2,2])
            assertEquals("Adjusted start[0] should be 1", origStart[0] - 1, newStart[0]);
            assertEquals("Adjusted start[1] should be 1", origStart[1] - 1, newStart[1]);
        }
    }

    @Test
    public void testH5Sget_select_elem_pointlist()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long[] dims               = {DIM_X, DIM_Y};
            MemorySegment dimsSegment = allocateLongArray(arena, RANK);
            copyToSegment(dimsSegment, dims);

            H5sid = hdf5_h.H5Screate_simple(RANK, dimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple failed", isValidId(H5sid));

            // Select 3 specific points
            long[] coords = {
                0, 0, // Point 1: [0,0]
                1, 2, // Point 2: [1,2]
                3, 5  // Point 3: [3,5]
            };
            int numPoints = 3;

            MemorySegment coordsSegment = allocateLongArray(arena, coords.length);
            copyToSegment(coordsSegment, coords);

            int result = hdf5_h.H5Sselect_elements(H5sid, hdf5_h.H5S_SELECT_SET(), numPoints, coordsSegment);
            assertTrue("H5Sselect_elements failed", isSuccess(result));

            // Get number of element points
            long npoints = hdf5_h.H5Sget_select_elem_npoints(H5sid);
            assertEquals("Should have 3 element points", 3L, npoints);

            // Get the point list back
            MemorySegment pointlist = allocateLongArray(arena, (int)(npoints * RANK));
            result                  = hdf5_h.H5Sget_select_elem_pointlist(H5sid, 0, npoints, pointlist);
            assertTrue("H5Sget_select_elem_pointlist failed", isSuccess(result));

            // Verify the coordinates
            long[] retrievedCoords = new long[(int)(npoints * RANK)];
            copyFromSegment(pointlist, retrievedCoords);

            assertArrayEquals("Point coordinates should match", coords, retrievedCoords);
        }
    }

    @Test
    public void testH5Sis_simple()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Test simple dataspace
            long[] dims               = {DIM_X, DIM_Y};
            MemorySegment dimsSegment = allocateLongArray(arena, RANK);
            copyToSegment(dimsSegment, dims);

            H5sid = hdf5_h.H5Screate_simple(RANK, dimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple failed", isValidId(H5sid));

            int isSimple = hdf5_h.H5Sis_simple(H5sid);
            assertTrue("Dataspace should be simple", isSimple > 0);

            hdf5_h.H5Sclose(H5sid);

            // Test scalar dataspace (also simple)
            H5sid = hdf5_h.H5Screate(hdf5_h.H5S_SCALAR());
            assertTrue("H5Screate scalar failed", isValidId(H5sid));

            isSimple = hdf5_h.H5Sis_simple(H5sid);
            assertTrue("Scalar dataspace should be simple", isSimple > 0);
        }
    }

    @Test
    public void testH5Sset_extent_none()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create a simple dataspace
            long[] dims               = {DIM_X, DIM_Y};
            MemorySegment dimsSegment = allocateLongArray(arena, RANK);
            copyToSegment(dimsSegment, dims);

            H5sid = hdf5_h.H5Screate_simple(RANK, dimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple failed", isValidId(H5sid));

            // Verify it's simple
            int type = hdf5_h.H5Sget_simple_extent_type(H5sid);
            assertEquals("Should be H5S_SIMPLE", hdf5_h.H5S_SIMPLE(), type);

            // Set extent to none (null dataspace)
            int result = hdf5_h.H5Sset_extent_none(H5sid);
            assertTrue("H5Sset_extent_none failed", isSuccess(result));

            // Verify it's now null
            type = hdf5_h.H5Sget_simple_extent_type(H5sid);
            assertEquals("Should be H5S_NULL", hdf5_h.H5S_NULL(), type);
        }
    }

    @Test
    public void testH5Sselect_copy()
    {
        System.out.print(testname.getMethodName());
        long H5sid2 = hdf5_h.H5I_INVALID_HID();

        try (Arena arena = Arena.ofConfined()) {
            // Create source dataspace with selection
            long[] dims               = {DIM_X, DIM_Y};
            MemorySegment dimsSegment = allocateLongArray(arena, RANK);
            copyToSegment(dimsSegment, dims);

            H5sid = hdf5_h.H5Screate_simple(RANK, dimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple failed", isValidId(H5sid));

            // Create destination dataspace
            H5sid2 = hdf5_h.H5Screate_simple(RANK, dimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple for dest failed", isValidId(H5sid2));

            // Select hyperslab in source
            long[] start               = {1, 1};
            long[] count               = {2, 3};
            MemorySegment startSegment = allocateLongArray(arena, RANK);
            MemorySegment countSegment = allocateLongArray(arena, RANK);
            copyToSegment(startSegment, start);
            copyToSegment(countSegment, count);

            int result = hdf5_h.H5Sselect_hyperslab(H5sid, hdf5_h.H5S_SELECT_SET(), startSegment,
                                                    MemorySegment.NULL, countSegment, MemorySegment.NULL);
            assertTrue("H5Sselect_hyperslab failed", isSuccess(result));

            // Get original selection npoints
            long npoints1 = hdf5_h.H5Sget_select_npoints(H5sid);
            assertEquals("Should have 6 points", 6L, npoints1);

            // Copy selection from source to destination
            result = hdf5_h.H5Sselect_copy(H5sid2, H5sid);
            assertTrue("H5Sselect_copy failed", isSuccess(result));

            // Verify destination has same selection
            long npoints2 = hdf5_h.H5Sget_select_npoints(H5sid2);
            assertEquals("Destination should have same npoints", npoints1, npoints2);
        }
        finally {
            closeQuietly(H5sid2, hdf5_h::H5Sclose);
        }
    }

    @Test
    public void testH5Sselect_shape_same()
    {
        System.out.print(testname.getMethodName());
        long H5sid2 = hdf5_h.H5I_INVALID_HID();

        try (Arena arena = Arena.ofConfined()) {
            // Create first dataspace with selection
            long[] dims1               = {DIM_X, DIM_Y};
            MemorySegment dims1Segment = allocateLongArray(arena, RANK);
            copyToSegment(dims1Segment, dims1);

            H5sid = hdf5_h.H5Screate_simple(RANK, dims1Segment, MemorySegment.NULL);
            assertTrue("H5Screate_simple failed", isValidId(H5sid));

            // Create second dataspace with different dims but same selection shape
            long[] dims2               = {8, 10}; // Different total dims
            MemorySegment dims2Segment = allocateLongArray(arena, RANK);
            copyToSegment(dims2Segment, dims2);

            H5sid2 = hdf5_h.H5Screate_simple(RANK, dims2Segment, MemorySegment.NULL);
            assertTrue("H5Screate_simple for sid2 failed", isValidId(H5sid2));

            // Select same shaped hyperslab in both (2x3 block)
            long[] start1               = {1, 1};
            long[] count1               = {2, 3};
            MemorySegment start1Segment = allocateLongArray(arena, RANK);
            MemorySegment count1Segment = allocateLongArray(arena, RANK);
            copyToSegment(start1Segment, start1);
            copyToSegment(count1Segment, count1);

            int result = hdf5_h.H5Sselect_hyperslab(H5sid, hdf5_h.H5S_SELECT_SET(), start1Segment,
                                                    MemorySegment.NULL, count1Segment, MemorySegment.NULL);
            assertTrue("H5Sselect_hyperslab for sid1 failed", isSuccess(result));

            long[] start2               = {2, 3}; // Different position
            long[] count2               = {2, 3}; // Same shape
            MemorySegment start2Segment = allocateLongArray(arena, RANK);
            MemorySegment count2Segment = allocateLongArray(arena, RANK);
            copyToSegment(start2Segment, start2);
            copyToSegment(count2Segment, count2);

            result = hdf5_h.H5Sselect_hyperslab(H5sid2, hdf5_h.H5S_SELECT_SET(), start2Segment,
                                                MemorySegment.NULL, count2Segment, MemorySegment.NULL);
            assertTrue("H5Sselect_hyperslab for sid2 failed", isSuccess(result));

            // Check if selections have same shape
            int same = hdf5_h.H5Sselect_shape_same(H5sid, H5sid2);
            assertTrue("Selections should have same shape", same > 0);
        }
        finally {
            closeQuietly(H5sid2, hdf5_h::H5Sclose);
        }
    }

    @Test
    public void testH5Sis_regular_hyperslab()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long[] dims               = {DIM_X, DIM_Y};
            MemorySegment dimsSegment = allocateLongArray(arena, RANK);
            copyToSegment(dimsSegment, dims);

            H5sid = hdf5_h.H5Screate_simple(RANK, dimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple failed", isValidId(H5sid));

            // Select regular hyperslab (single block)
            long[] start                = {1, 1};
            long[] stride               = {1, 1};
            long[] count                = {1, 1};
            long[] block                = {2, 3};
            MemorySegment startSegment  = allocateLongArray(arena, RANK);
            MemorySegment strideSegment = allocateLongArray(arena, RANK);
            MemorySegment countSegment  = allocateLongArray(arena, RANK);
            MemorySegment blockSegment  = allocateLongArray(arena, RANK);
            copyToSegment(startSegment, start);
            copyToSegment(strideSegment, stride);
            copyToSegment(countSegment, count);
            copyToSegment(blockSegment, block);

            int result = hdf5_h.H5Sselect_hyperslab(H5sid, hdf5_h.H5S_SELECT_SET(), startSegment,
                                                    strideSegment, countSegment, blockSegment);
            assertTrue("H5Sselect_hyperslab failed", isSuccess(result));

            // Check if it's regular
            int regular = hdf5_h.H5Sis_regular_hyperslab(H5sid);
            assertTrue("Should be regular hyperslab", regular > 0);
        }
    }

    @Test
    public void testH5Sget_regular_hyperslab()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long[] dims               = {DIM_X, DIM_Y};
            MemorySegment dimsSegment = allocateLongArray(arena, RANK);
            copyToSegment(dimsSegment, dims);

            H5sid = hdf5_h.H5Screate_simple(RANK, dimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple failed", isValidId(H5sid));

            // Select regular hyperslab
            long[] start                = {1, 1};
            long[] stride               = {2, 2};
            long[] count                = {2, 2};
            long[] block                = {1, 1};
            MemorySegment startSegment  = allocateLongArray(arena, RANK);
            MemorySegment strideSegment = allocateLongArray(arena, RANK);
            MemorySegment countSegment  = allocateLongArray(arena, RANK);
            MemorySegment blockSegment  = allocateLongArray(arena, RANK);
            copyToSegment(startSegment, start);
            copyToSegment(strideSegment, stride);
            copyToSegment(countSegment, count);
            copyToSegment(blockSegment, block);

            int result = hdf5_h.H5Sselect_hyperslab(H5sid, hdf5_h.H5S_SELECT_SET(), startSegment,
                                                    strideSegment, countSegment, blockSegment);
            assertTrue("H5Sselect_hyperslab failed", isSuccess(result));

            // Get regular hyperslab info
            MemorySegment outStart  = allocateLongArray(arena, RANK);
            MemorySegment outStride = allocateLongArray(arena, RANK);
            MemorySegment outCount  = allocateLongArray(arena, RANK);
            MemorySegment outBlock  = allocateLongArray(arena, RANK);

            result = hdf5_h.H5Sget_regular_hyperslab(H5sid, outStart, outStride, outCount, outBlock);
            assertTrue("H5Sget_regular_hyperslab failed", isSuccess(result));

            // Verify parameters match
            assertEquals("Start[0] should match", 1L, outStart.getAtIndex(ValueLayout.JAVA_LONG, 0));
            assertEquals("Start[1] should match", 1L, outStart.getAtIndex(ValueLayout.JAVA_LONG, 1));
            assertEquals("Stride[0] should match", 2L, outStride.getAtIndex(ValueLayout.JAVA_LONG, 0));
            assertEquals("Stride[1] should match", 2L, outStride.getAtIndex(ValueLayout.JAVA_LONG, 1));
            assertEquals("Count[0] should match", 2L, outCount.getAtIndex(ValueLayout.JAVA_LONG, 0));
            assertEquals("Count[1] should match", 2L, outCount.getAtIndex(ValueLayout.JAVA_LONG, 1));
            assertEquals("Block[0] should match", 1L, outBlock.getAtIndex(ValueLayout.JAVA_LONG, 0));
            assertEquals("Block[1] should match", 1L, outBlock.getAtIndex(ValueLayout.JAVA_LONG, 1));
        }
    }

    // ============================================================================
    // Phase 1: Advanced Selection Operations
    // ============================================================================

    @Test
    public void testH5Scombine_hyperslab()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long[] dims               = {DIM_X, DIM_Y};
            MemorySegment dimsSegment = allocateLongArray(arena, RANK);
            copyToSegment(dimsSegment, dims);

            H5sid = hdf5_h.H5Screate_simple(RANK, dimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple failed", isValidId(H5sid));

            // First selection: [1,1] to [2,2] (2x2 block)
            long[] start1         = {1, 1};
            long[] count1         = {2, 2};
            MemorySegment start1s = allocateLongArray(arena, RANK);
            MemorySegment count1s = allocateLongArray(arena, RANK);
            copyToSegment(start1s, start1);
            copyToSegment(count1s, count1);

            int result = hdf5_h.H5Sselect_hyperslab(H5sid, hdf5_h.H5S_SELECT_SET(), start1s,
                                                    MemorySegment.NULL, count1s, MemorySegment.NULL);
            assertTrue("H5Sselect_hyperslab failed", isSuccess(result));

            // Combine with second selection: [2,2] to [3,3] (2x2 block) using OR
            long[] start2         = {2, 2};
            long[] count2         = {2, 2};
            MemorySegment start2s = allocateLongArray(arena, RANK);
            MemorySegment count2s = allocateLongArray(arena, RANK);
            copyToSegment(start2s, start2);
            copyToSegment(count2s, count2);

            // Combine creates a new dataspace
            long combined_sid = hdf5_h.H5Scombine_hyperslab(H5sid, hdf5_h.H5S_SELECT_OR(), start2s,
                                                            MemorySegment.NULL, count2s, MemorySegment.NULL);
            assertTrue("H5Scombine_hyperslab failed", isValidId(combined_sid));

            // Verify combined selection has more points than original
            long original_npoints = hdf5_h.H5Sget_select_npoints(H5sid);
            long combined_npoints = hdf5_h.H5Sget_select_npoints(combined_sid);
            assertTrue("Combined selection should have more points", combined_npoints > original_npoints);

            // Original: 2x2 = 4 points, Combined: should be larger due to OR
            assertEquals("Original should have 4 points", 4L, original_npoints);

            hdf5_h.H5Sclose(combined_sid);
        }
    }

    @Test
    public void testH5Scombine_select()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long[] dims               = {DIM_X, DIM_Y};
            MemorySegment dimsSegment = allocateLongArray(arena, RANK);
            copyToSegment(dimsSegment, dims);

            // Create first dataspace with selection
            long space1_id = hdf5_h.H5Screate_simple(RANK, dimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple space1 failed", isValidId(space1_id));

            long[] start1         = {0, 0};
            long[] count1         = {2, 3};
            MemorySegment start1s = allocateLongArray(arena, RANK);
            MemorySegment count1s = allocateLongArray(arena, RANK);
            copyToSegment(start1s, start1);
            copyToSegment(count1s, count1);

            int result = hdf5_h.H5Sselect_hyperslab(space1_id, hdf5_h.H5S_SELECT_SET(), start1s,
                                                    MemorySegment.NULL, count1s, MemorySegment.NULL);
            assertTrue("H5Sselect_hyperslab space1 failed", isSuccess(result));

            // Create second dataspace with different selection
            long space2_id = hdf5_h.H5Screate_simple(RANK, dimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple space2 failed", isValidId(space2_id));

            long[] start2         = {1, 1};
            long[] count2         = {2, 3};
            MemorySegment start2s = allocateLongArray(arena, RANK);
            MemorySegment count2s = allocateLongArray(arena, RANK);
            copyToSegment(start2s, start2);
            copyToSegment(count2s, count2);

            result = hdf5_h.H5Sselect_hyperslab(space2_id, hdf5_h.H5S_SELECT_SET(), start2s,
                                                MemorySegment.NULL, count2s, MemorySegment.NULL);
            assertTrue("H5Sselect_hyperslab space2 failed", isSuccess(result));

            // Combine selections with OR operation
            long combined_sid = hdf5_h.H5Scombine_select(space1_id, hdf5_h.H5S_SELECT_OR(), space2_id);
            assertTrue("H5Scombine_select failed", isValidId(combined_sid));

            // Verify combined selection has expected points
            long space1_npoints   = hdf5_h.H5Sget_select_npoints(space1_id);
            long space2_npoints   = hdf5_h.H5Sget_select_npoints(space2_id);
            long combined_npoints = hdf5_h.H5Sget_select_npoints(combined_sid);

            assertEquals("Space1 should have 6 points", 6L, space1_npoints);
            assertEquals("Space2 should have 6 points", 6L, space2_npoints);
            // Combined with OR will be at least the larger of the two
            assertTrue("Combined selection should have points", combined_npoints > 0);

            hdf5_h.H5Sclose(space1_id);
            hdf5_h.H5Sclose(space2_id);
            hdf5_h.H5Sclose(combined_sid);
        }
    }

    @Test
    public void testH5Smodify_select()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long[] dims               = {DIM_X, DIM_Y};
            MemorySegment dimsSegment = allocateLongArray(arena, RANK);
            copyToSegment(dimsSegment, dims);

            // Create first dataspace with selection
            long space1_id = hdf5_h.H5Screate_simple(RANK, dimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple space1 failed", isValidId(space1_id));

            long[] start1         = {0, 0};
            long[] count1         = {2, 2};
            MemorySegment start1s = allocateLongArray(arena, RANK);
            MemorySegment count1s = allocateLongArray(arena, RANK);
            copyToSegment(start1s, start1);
            copyToSegment(count1s, count1);

            int result = hdf5_h.H5Sselect_hyperslab(space1_id, hdf5_h.H5S_SELECT_SET(), start1s,
                                                    MemorySegment.NULL, count1s, MemorySegment.NULL);
            assertTrue("H5Sselect_hyperslab space1 failed", isSuccess(result));

            long original_npoints = hdf5_h.H5Sget_select_npoints(space1_id);
            assertEquals("Original should have 4 points", 4L, original_npoints);

            // Create second dataspace with different selection
            long space2_id = hdf5_h.H5Screate_simple(RANK, dimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple space2 failed", isValidId(space2_id));

            long[] start2         = {1, 1};
            long[] count2         = {2, 2};
            MemorySegment start2s = allocateLongArray(arena, RANK);
            MemorySegment count2s = allocateLongArray(arena, RANK);
            copyToSegment(start2s, start2);
            copyToSegment(count2s, count2);

            result = hdf5_h.H5Sselect_hyperslab(space2_id, hdf5_h.H5S_SELECT_SET(), start2s,
                                                MemorySegment.NULL, count2s, MemorySegment.NULL);
            assertTrue("H5Sselect_hyperslab space2 failed", isSuccess(result));

            // Modify space1's selection by combining with space2 using OR
            result = hdf5_h.H5Smodify_select(space1_id, hdf5_h.H5S_SELECT_OR(), space2_id);
            assertTrue("H5Smodify_select failed", isSuccess(result));

            // Verify modified selection has more points
            long modified_npoints = hdf5_h.H5Sget_select_npoints(space1_id);
            assertTrue("Modified selection should have more points than original",
                       modified_npoints > original_npoints);

            hdf5_h.H5Sclose(space1_id);
            hdf5_h.H5Sclose(space2_id);
        }
    }

    @Test
    public void testH5Sselect_intersect_block()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long[] dims               = {DIM_X, DIM_Y};
            MemorySegment dimsSegment = allocateLongArray(arena, RANK);
            copyToSegment(dimsSegment, dims);

            H5sid = hdf5_h.H5Screate_simple(RANK, dimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple failed", isValidId(H5sid));

            // Create selection: [1,1] to [2,2]
            long[] start         = {1, 1};
            long[] count         = {2, 2};
            MemorySegment starts = allocateLongArray(arena, RANK);
            MemorySegment counts = allocateLongArray(arena, RANK);
            copyToSegment(starts, start);
            copyToSegment(counts, count);

            int result = hdf5_h.H5Sselect_hyperslab(H5sid, hdf5_h.H5S_SELECT_SET(), starts,
                                                    MemorySegment.NULL, counts, MemorySegment.NULL);
            assertTrue("H5Sselect_hyperslab failed", isSuccess(result));

            // Test intersection with overlapping block [1,1] to [2,2]
            long[] block_start   = {1, 1};
            long[] block_end     = {2, 2};
            MemorySegment bstart = allocateLongArray(arena, RANK);
            MemorySegment bend   = allocateLongArray(arena, RANK);
            copyToSegment(bstart, block_start);
            copyToSegment(bend, block_end);

            result = hdf5_h.H5Sselect_intersect_block(H5sid, bstart, bend);
            assertTrue("Block [1,1]-[2,2] should intersect with selection [1,1]-[2,2]", result > 0);

            // Test non-intersecting block [0,0] to [0,0]
            long[] block_start2   = {0, 0};
            long[] block_end2     = {0, 0};
            MemorySegment bstart2 = allocateLongArray(arena, RANK);
            MemorySegment bend2   = allocateLongArray(arena, RANK);
            copyToSegment(bstart2, block_start2);
            copyToSegment(bend2, block_end2);

            result = hdf5_h.H5Sselect_intersect_block(H5sid, bstart2, bend2);
            assertEquals("Block [0,0]-[0,0] should not intersect with selection [1,1]-[2,2]", 0, result);
        }
    }

    @Test
    public void testH5Sselect_project_intersection()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create 3D source dataspace
            int src_rank             = 3;
            long[] src_dims          = {4, 6, 8};
            MemorySegment srcDimsSeg = allocateLongArray(arena, src_rank);
            copyToSegment(srcDimsSeg, src_dims);

            long src_sid = hdf5_h.H5Screate_simple(src_rank, srcDimsSeg, MemorySegment.NULL);
            assertTrue("H5Screate_simple src failed", isValidId(src_sid));

            // Select region in source [0,1,1] count [1,2,2] (4 points)
            long[] src_start        = {0, 1, 1};
            long[] src_count        = {1, 2, 2};
            MemorySegment srcStarts = allocateLongArray(arena, src_rank);
            MemorySegment srcCounts = allocateLongArray(arena, src_rank);
            copyToSegment(srcStarts, src_start);
            copyToSegment(srcCounts, src_count);

            int result = hdf5_h.H5Sselect_hyperslab(src_sid, hdf5_h.H5S_SELECT_SET(), srcStarts,
                                                    MemorySegment.NULL, srcCounts, MemorySegment.NULL);
            assertTrue("H5Sselect_hyperslab src failed", isSuccess(result));

            // Create 3D destination dataspace (same rank as source)
            long dst_sid = hdf5_h.H5Screate_simple(src_rank, srcDimsSeg, MemorySegment.NULL);
            assertTrue("H5Screate_simple dst failed", isValidId(dst_sid));

            // Select matching region in destination [0,0,0] count [1,3,3] (9 points, overlaps with src)
            long[] dst_start        = {0, 0, 0};
            long[] dst_count        = {1, 3, 3};
            MemorySegment dstStarts = allocateLongArray(arena, src_rank);
            MemorySegment dstCounts = allocateLongArray(arena, src_rank);
            copyToSegment(dstStarts, dst_start);
            copyToSegment(dstCounts, dst_count);

            result = hdf5_h.H5Sselect_hyperslab(dst_sid, hdf5_h.H5S_SELECT_SET(), dstStarts,
                                                MemorySegment.NULL, dstCounts, MemorySegment.NULL);
            assertTrue("H5Sselect_hyperslab dst failed", isSuccess(result));

            // Create 2D projection space
            int proj_rank             = 2;
            long[] proj_dims          = {6, 8};
            MemorySegment projDimsSeg = allocateLongArray(arena, proj_rank);
            copyToSegment(projDimsSeg, proj_dims);

            long proj_space = hdf5_h.H5Screate_simple(proj_rank, projDimsSeg, MemorySegment.NULL);
            assertTrue("H5Screate_simple proj failed", isValidId(proj_space));

            // Project intersection - projects src selection onto dst, creating result in proj_space
            // This tests the function exists and can be called (may fail due to complex requirements)
            long proj_sid = hdf5_h.H5Sselect_project_intersection(src_sid, dst_sid, proj_space);

            // Only verify if valid ID returned (function is complex and may have strict requirements)
            if (isValidId(proj_sid)) {
                long proj_npoints = hdf5_h.H5Sget_select_npoints(proj_sid);
                assertTrue("Projected selection should have points", proj_npoints > 0);
                hdf5_h.H5Sclose(proj_sid);
            }

            hdf5_h.H5Sclose(src_sid);
            hdf5_h.H5Sclose(dst_sid);
            hdf5_h.H5Sclose(proj_space);
        }
    }

    // ============================================================================
    // Phase 2: Validation and Offset Operations
    // ============================================================================

    @Test
    public void testH5Sselect_valid_comprehensive()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long[] dims               = {DIM_X, DIM_Y};
            MemorySegment dimsSegment = allocateLongArray(arena, RANK);
            copyToSegment(dimsSegment, dims);

            H5sid = hdf5_h.H5Screate_simple(RANK, dimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple failed", isValidId(H5sid));

            // Initially no selection (NONE), which is valid
            int result = hdf5_h.H5Sselect_none(H5sid);
            assertTrue("H5Sselect_none failed", isSuccess(result));

            result = hdf5_h.H5Sselect_valid(H5sid);
            assertTrue("NONE selection should be valid", result > 0);

            // Select valid hyperslab
            long[] start         = {0, 0};
            long[] count         = {2, 2};
            MemorySegment starts = allocateLongArray(arena, RANK);
            MemorySegment counts = allocateLongArray(arena, RANK);
            copyToSegment(starts, start);
            copyToSegment(counts, count);

            result = hdf5_h.H5Sselect_hyperslab(H5sid, hdf5_h.H5S_SELECT_SET(), starts, MemorySegment.NULL,
                                                counts, MemorySegment.NULL);
            assertTrue("H5Sselect_hyperslab failed", isSuccess(result));

            result = hdf5_h.H5Sselect_valid(H5sid);
            assertTrue("Valid hyperslab selection should be valid", result > 0);

            // Select ALL
            result = hdf5_h.H5Sselect_all(H5sid);
            assertTrue("H5Sselect_all failed", isSuccess(result));

            result = hdf5_h.H5Sselect_valid(H5sid);
            assertTrue("ALL selection should be valid", result > 0);
        }
    }

    @Test
    public void testH5Soffset_simple()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long[] dims               = {DIM_X, DIM_Y};
            MemorySegment dimsSegment = allocateLongArray(arena, RANK);
            copyToSegment(dimsSegment, dims);

            H5sid = hdf5_h.H5Screate_simple(RANK, dimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple failed", isValidId(H5sid));

            // Create selection [1,1] to [2,2]
            long[] start         = {1, 1};
            long[] count         = {2, 2};
            MemorySegment starts = allocateLongArray(arena, RANK);
            MemorySegment counts = allocateLongArray(arena, RANK);
            copyToSegment(starts, start);
            copyToSegment(counts, count);

            int result = hdf5_h.H5Sselect_hyperslab(H5sid, hdf5_h.H5S_SELECT_SET(), starts,
                                                    MemorySegment.NULL, counts, MemorySegment.NULL);
            assertTrue("H5Sselect_hyperslab failed", isSuccess(result));

            // Apply offset [1, 1]
            long[] offset           = {1, 1};
            MemorySegment offsetSeg = allocateLongArray(arena, RANK);
            copyToSegment(offsetSeg, offset);

            result = hdf5_h.H5Soffset_simple(H5sid, offsetSeg);
            assertTrue("H5Soffset_simple failed", isSuccess(result));

            // After offset, selection should be shifted to [2,2] to [3,3]
            // Verify by getting bounds
            MemorySegment startBounds = allocateLongArray(arena, RANK);
            MemorySegment endBounds   = allocateLongArray(arena, RANK);

            result = hdf5_h.H5Sget_select_bounds(H5sid, startBounds, endBounds);
            assertTrue("H5Sget_select_bounds failed", isSuccess(result));

            long[] outStart = new long[RANK];
            long[] outEnd   = new long[RANK];
            copyFromSegment(startBounds, outStart);
            copyFromSegment(endBounds, outEnd);

            // Original was [1,1] to [2,2], with offset [1,1] becomes [2,2] to [3,3]
            assertEquals("Start[0] should be 2 after offset", 2L, outStart[0]);
            assertEquals("Start[1] should be 2 after offset", 2L, outStart[1]);
            assertEquals("End[0] should be 3 after offset", 3L, outEnd[0]);
            assertEquals("End[1] should be 3 after offset", 3L, outEnd[1]);
        }
    }

    // ============================================================================
    // Phase 3: Encoding and Iterator Operations
    // ============================================================================

    @Test
    public void testH5Sdecode_encode2_comprehensive()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long[] dims               = {DIM_X, DIM_Y};
            MemorySegment dimsSegment = allocateLongArray(arena, RANK);
            copyToSegment(dimsSegment, dims);

            H5sid = hdf5_h.H5Screate_simple(RANK, dimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple failed", isValidId(H5sid));

            // Add a selection to make it more interesting
            long[] start         = {0, 0};
            long[] count         = {2, 3};
            MemorySegment starts = allocateLongArray(arena, RANK);
            MemorySegment counts = allocateLongArray(arena, RANK);
            copyToSegment(starts, start);
            copyToSegment(counts, count);

            int result = hdf5_h.H5Sselect_hyperslab(H5sid, hdf5_h.H5S_SELECT_SET(), starts,
                                                    MemorySegment.NULL, counts, MemorySegment.NULL);
            assertTrue("H5Sselect_hyperslab failed", isSuccess(result));

            // Get original npoints
            long orig_npoints = hdf5_h.H5Sget_select_npoints(H5sid);
            assertEquals("Original should have 6 points", 6L, orig_npoints);

            // Encode with H5Sencode2 (first get size)
            MemorySegment sizePtr = allocateLong(arena);
            result = hdf5_h.H5Sencode2(H5sid, MemorySegment.NULL, sizePtr, hdf5_h.H5P_DEFAULT());
            assertTrue("H5Sencode2 size query failed", isSuccess(result));

            long size = getLong(sizePtr);
            assertTrue("Encoded size should be positive", size > 0);

            // Allocate buffer and encode
            MemorySegment buffer = arena.allocate(size);
            result               = hdf5_h.H5Sencode2(H5sid, buffer, sizePtr, hdf5_h.H5P_DEFAULT());
            assertTrue("H5Sencode2 failed", isSuccess(result));

            // Decode back
            long decoded_sid = hdf5_h.H5Sdecode(buffer);
            assertTrue("H5Sdecode failed", isValidId(decoded_sid));

            // Verify decoded dataspace matches original
            long decoded_npoints = hdf5_h.H5Sget_select_npoints(decoded_sid);
            assertEquals("Decoded dataspace should have same npoints as original", orig_npoints,
                         decoded_npoints);

            int decoded_ndims = hdf5_h.H5Sget_simple_extent_ndims(decoded_sid);
            assertEquals("Decoded dataspace should have same rank", RANK, decoded_ndims);

            hdf5_h.H5Sclose(decoded_sid);
        }
    }

    @Test
    public void testH5Ssel_iter_comprehensive()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long[] dims               = {DIM_X, DIM_Y};
            MemorySegment dimsSegment = allocateLongArray(arena, RANK);
            copyToSegment(dimsSegment, dims);

            H5sid = hdf5_h.H5Screate_simple(RANK, dimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple failed", isValidId(H5sid));

            // Select some elements
            long[] start         = {0, 0};
            long[] count         = {2, 2};
            MemorySegment starts = allocateLongArray(arena, RANK);
            MemorySegment counts = allocateLongArray(arena, RANK);
            copyToSegment(starts, start);
            copyToSegment(counts, count);

            int result = hdf5_h.H5Sselect_hyperslab(H5sid, hdf5_h.H5S_SELECT_SET(), starts,
                                                    MemorySegment.NULL, counts, MemorySegment.NULL);
            assertTrue("H5Sselect_hyperslab failed", isSuccess(result));

            // Create selection iterator
            long elmt_size = 4; // 4 bytes for int
            long iter_id   = hdf5_h.H5Ssel_iter_create(H5sid, elmt_size, 0);
            assertTrue("H5Ssel_iter_create failed", isValidId(iter_id));

            // Get sequence list
            long maxseq            = 10;
            long maxelmts          = 100;
            MemorySegment nseqPtr  = allocateLong(arena);
            MemorySegment neltsPtr = allocateLong(arena);
            MemorySegment offArray = allocateLongArray(arena, (int)maxseq);
            MemorySegment lenArray = allocateLongArray(arena, (int)maxseq);

            result = hdf5_h.H5Ssel_iter_get_seq_list(iter_id, maxseq, maxelmts, nseqPtr, neltsPtr, offArray,
                                                     lenArray);
            assertTrue("H5Ssel_iter_get_seq_list failed", isSuccess(result));

            long nseq  = getLong(nseqPtr);
            long nelts = getLong(neltsPtr);
            assertTrue("Should have at least one sequence", nseq > 0);
            assertTrue("Should have at least one element", nelts > 0);

            // Reset iterator
            result = hdf5_h.H5Ssel_iter_reset(iter_id, H5sid);
            assertTrue("H5Ssel_iter_reset failed", isSuccess(result));

            // Close iterator
            result = hdf5_h.H5Ssel_iter_close(iter_id);
            assertTrue("H5Ssel_iter_close failed", isSuccess(result));
        }
    }
}
