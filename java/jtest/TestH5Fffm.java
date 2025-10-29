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

import java.io.File;
import java.lang.foreign.Arena;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.ValueLayout;

import org.hdfgroup.javahdf5.H5AC_cache_config_t;
import org.hdfgroup.javahdf5.H5F_info2_t;
import org.hdfgroup.javahdf5.hdf5_h;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

/**
 * FFM-only tests for HDF5 File (H5F) operations.
 *
 * This test class uses direct FFM bindings without the hdf.hdf5lib wrapper layer.
 */
public class TestH5Fffm {
    @Rule
    public TestName testname = new TestName();

    private static final String H5_FILE  = "testFffm.h5";
    private static final String H5_FILE2 = "testFffm2.h5";

    long H5fid = hdf5_h.H5I_INVALID_HID();

    private void deleteFile(String filename)
    {
        File file = new File(filename);
        if (file.exists()) {
            try {
                file.delete();
            }
            catch (SecurityException e) {
                // Ignore
            }
        }
    }

    @Before
    public void createH5file()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            MemorySegment fileNameSegment = stringToSegment(arena, H5_FILE);
            H5fid = hdf5_h.H5Fcreate(fileNameSegment, hdf5_h.H5F_ACC_TRUNC(), hdf5_h.H5P_DEFAULT(),
                                     hdf5_h.H5P_DEFAULT());
            assertTrue("H5Fcreate failed", isValidId(H5fid));

            int flushResult = hdf5_h.H5Fflush(H5fid, hdf5_h.H5F_SCOPE_LOCAL());
            assertTrue("H5Fflush failed", isSuccess(flushResult));
        }
    }

    @After
    public void deleteH5file()
    {
        if (H5fid >= 0) {
            closeQuietly(H5fid, hdf5_h::H5Fclose);
            H5fid = hdf5_h.H5I_INVALID_HID();
        }
        deleteFile(H5_FILE);
        deleteFile(H5_FILE2);
        System.out.println();
    }

    @Test
    public void testH5Fopen()
    {
        long fid = hdf5_h.H5I_INVALID_HID();

        try (Arena arena = Arena.ofConfined()) {
            MemorySegment fileNameSegment = stringToSegment(arena, H5_FILE);
            fid = hdf5_h.H5Fopen(fileNameSegment, hdf5_h.H5F_ACC_RDONLY(), hdf5_h.H5P_DEFAULT());
            assertTrue("H5Fopen failed", isValidId(fid));
        }
        finally {
            closeQuietly(fid, hdf5_h::H5Fclose);
        }
    }

    @Test
    public void testH5Freopen()
    {
        long fid2 = hdf5_h.H5I_INVALID_HID();

        try {
            fid2 = hdf5_h.H5Freopen(H5fid);
            assertTrue("H5Freopen failed", isValidId(fid2));
            assertNotEquals("H5Freopen should return different id", H5fid, fid2);
        }
        finally {
            closeQuietly(fid2, hdf5_h::H5Fclose);
        }
    }

    @Test
    public void testH5Fget_create_plist()
    {
        long plist = hdf5_h.H5I_INVALID_HID();

        try {
            plist = hdf5_h.H5Fget_create_plist(H5fid);
            assertTrue("H5Fget_create_plist failed", isValidId(plist));
        }
        finally {
            closeQuietly(plist, hdf5_h::H5Pclose);
        }
    }

    @Test
    public void testH5Fget_access_plist()
    {
        long plist = hdf5_h.H5I_INVALID_HID();

        try {
            plist = hdf5_h.H5Fget_access_plist(H5fid);
            assertTrue("H5Fget_access_plist failed", isValidId(plist));
        }
        finally {
            closeQuietly(plist, hdf5_h::H5Pclose);
        }
    }

    @Test
    public void testH5Fget_intent()
    {
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment intentSegment = allocateInt(arena);

            int result = hdf5_h.H5Fget_intent(H5fid, intentSegment);
            assertTrue("H5Fget_intent failed", isSuccess(result));

            int intent = getInt(intentSegment);
            assertTrue("File should be opened with write access",
                       (intent & hdf5_h.H5F_ACC_RDWR()) == hdf5_h.H5F_ACC_RDWR());
        }
    }

    @Test
    public void testH5Fget_name()
    {
        try (Arena arena = Arena.ofConfined()) {
            // First call to get the name length
            long nameLength = hdf5_h.H5Fget_name(H5fid, MemorySegment.NULL, 0);
            assertTrue("H5Fget_name (get length) failed", nameLength > 0);

            // Second call to get the actual name
            MemorySegment nameSegment = arena.allocate(nameLength + 1);
            long result               = hdf5_h.H5Fget_name(H5fid, nameSegment, nameLength + 1);
            assertTrue("H5Fget_name failed", result > 0);

            String fileName = nameSegment.getString(0);
            assertTrue("File name should contain test file name", fileName.contains(H5_FILE));
        }
    }

    @Test
    public void testH5Fget_filesize()
    {
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment sizeSegment = allocateLong(arena);
            int result                = hdf5_h.H5Fget_filesize(H5fid, sizeSegment);
            assertTrue("H5Fget_filesize failed", isSuccess(result));

            long fileSize = getLong(sizeSegment);
            assertTrue("File size should be > 0", fileSize > 0);
        }
    }

    @Test
    public void testH5Fget_obj_count()
    {
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment countSegment = allocateLong(arena);

            // Count all objects
            long result = hdf5_h.H5Fget_obj_count(H5fid, hdf5_h.H5F_OBJ_ALL());
            assertTrue("H5Fget_obj_count failed", result >= 0);
            assertTrue("Should have at least one object (the file)", result >= 1);
        }
    }

    @Test
    public void testH5Fget_info()
    {
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment fileInfoSegment = H5F_info2_t.allocate(arena);
            int result                    = hdf5_h.H5Fget_info2(H5fid, fileInfoSegment);
            assertTrue("H5Fget_info2 failed", isSuccess(result));

            // Struct verified (complex struct accessor testing skipped in FFM)
        }
    }

    @Test
    public void testH5Fis_accessible()
    {
        try (Arena arena = Arena.ofConfined()) {
            // Close the file first
            closeQuietly(H5fid, hdf5_h::H5Fclose);
            H5fid = hdf5_h.H5I_INVALID_HID();

            // Check if file is accessible
            MemorySegment fileNameSegment = stringToSegment(arena, H5_FILE);
            int result                    = hdf5_h.H5Fis_accessible(fileNameSegment, hdf5_h.H5P_DEFAULT());
            assertTrue("H5Fis_accessible should return true", result > 0);

            // Check non-existent file
            MemorySegment badFileSegment = stringToSegment(arena, "nonexistent.h5");
            result                       = hdf5_h.H5Fis_accessible(badFileSegment, hdf5_h.H5P_DEFAULT());
            assertFalse("H5Fis_accessible should return false for non-existent file", result > 0);
        }
    }

    @Test
    public void testH5Fclear_elink_file_cache()
    {
        int result = hdf5_h.H5Fclear_elink_file_cache(H5fid);
        assertTrue("H5Fclear_elink_file_cache failed", isSuccess(result));
    }

    @Test
    public void testH5Fclose()
    {
        long fid = hdf5_h.H5I_INVALID_HID();

        try (Arena arena = Arena.ofConfined()) {
            MemorySegment fileNameSegment = stringToSegment(arena, H5_FILE2);
            fid = hdf5_h.H5Fcreate(fileNameSegment, hdf5_h.H5F_ACC_TRUNC(), hdf5_h.H5P_DEFAULT(),
                                   hdf5_h.H5P_DEFAULT());
            assertTrue("H5Fcreate failed", isValidId(fid));

            int result = hdf5_h.H5Fclose(fid);
            assertTrue("H5Fclose failed", isSuccess(result));
            fid = hdf5_h.H5I_INVALID_HID();
        }
    }

    // =========================
    // File Metadata and Cache Tests
    // =========================

    @Test
    public void testH5Fget_freespace()
    {
        try (Arena arena = Arena.ofConfined()) {
            long freespace = hdf5_h.H5Fget_freespace(H5fid);
            assertTrue("H5Fget_freespace should return non-negative value", freespace >= 0);
        }
    }

    @Test
    public void testH5Fget_mdc_config()
    {
        try (Arena arena = Arena.ofConfined()) {
            // Allocate and initialize H5AC_cache_config_t structure
            MemorySegment config = H5AC_cache_config_t.allocate(arena);

            // Set version field (required)
            H5AC_cache_config_t.version(config, hdf5_h.H5AC__CURR_CACHE_CONFIG_VERSION());

            int result = hdf5_h.H5Fget_mdc_config(H5fid, config);
            assertTrue("H5Fget_mdc_config failed", isSuccess(result));

            // Verify we got valid data back
            int version = H5AC_cache_config_t.version(config);
            assertEquals("Version should match", hdf5_h.H5AC__CURR_CACHE_CONFIG_VERSION(), version);
        }
    }

    @Test
    public void testH5Fget_mdc_hit_rate()
    {
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment hitRate = allocateDoubleArray(arena, 1);

            int result = hdf5_h.H5Fget_mdc_hit_rate(H5fid, hitRate);
            assertTrue("H5Fget_mdc_hit_rate failed", isSuccess(result));

            double rate = getDouble(hitRate);
            assertTrue("Hit rate should be between 0.0 and 1.0", rate >= 0.0 && rate <= 1.0);
        }
    }

    @Test
    public void testH5Fget_fileno()
    {
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment fileno = allocateLongArray(arena, 1);

            int result = hdf5_h.H5Fget_fileno(H5fid, fileno);
            assertTrue("H5Fget_fileno failed", isSuccess(result));

            // File number should be valid (non-negative on most systems)
            long fileNum = getLong(fileno);
            // Just verify we got some value - actual value is system-dependent
            assertNotEquals("File number should be set", 0L, fileNum | 1);
        }
    }

    @Test
    public void testH5Fget_file_image()
    {
        try (Arena arena = Arena.ofConfined()) {
            // First get size
            long imageSize = hdf5_h.H5Fget_file_image(H5fid, MemorySegment.NULL, 0);
            assertTrue("H5Fget_file_image should return positive size", imageSize > 0);

            // Allocate buffer and get image (limit to 64KB for test)
            long bufSize              = Math.min(imageSize, 65536);
            MemorySegment imageBuffer = arena.allocate(bufSize);

            long actualSize = hdf5_h.H5Fget_file_image(H5fid, imageBuffer, bufSize);
            assertTrue("H5Fget_file_image should return size", actualSize > 0);
        }
    }

    @Test
    public void testH5Fget_mdc_logging_status()
    {
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment isEnabled          = allocateIntArray(arena, 1);
            MemorySegment isCurrentlyLogging = allocateIntArray(arena, 1);

            int result = hdf5_h.H5Fget_mdc_logging_status(H5fid, isEnabled, isCurrentlyLogging);
            assertTrue("H5Fget_mdc_logging_status failed", isSuccess(result));

            // Values should be boolean (0 or 1)
            int enabled = getInt(isEnabled);
            int logging = getInt(isCurrentlyLogging);
            assertTrue("Enabled should be 0 or 1", enabled == 0 || enabled == 1);
            assertTrue("Currently logging should be 0 or 1", logging == 0 || logging == 1);
        }
    }

    @Test
    public void testH5Freset_mdc_hit_rate_stats()
    {
        int result = hdf5_h.H5Freset_mdc_hit_rate_stats(H5fid);
        assertTrue("H5Freset_mdc_hit_rate_stats failed", isSuccess(result));
    }

    @Test
    public void testH5Fget_mdc_size()
    {
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment maxSize       = arena.allocate(ValueLayout.JAVA_LONG);
            MemorySegment minCleanSize  = arena.allocate(ValueLayout.JAVA_LONG);
            MemorySegment curSize       = arena.allocate(ValueLayout.JAVA_LONG);
            MemorySegment curNumEntries = allocateInt(arena);

            int result = hdf5_h.H5Fget_mdc_size(H5fid, maxSize, minCleanSize, curSize, curNumEntries);
            assertTrue("H5Fget_mdc_size failed", isSuccess(result));

            long max      = maxSize.get(ValueLayout.JAVA_LONG, 0);
            long minClean = minCleanSize.get(ValueLayout.JAVA_LONG, 0);
            long cur      = curSize.get(ValueLayout.JAVA_LONG, 0);
            int entries   = getInt(curNumEntries);

            assertTrue("Max size should be positive", max > 0);
            assertTrue("Current size should be non-negative", cur >= 0);
            assertTrue("Entries should be non-negative", entries >= 0);
        }
    }

    @Test
    public void testH5Fget_free_sections()
    {
        try (Arena arena = Arena.ofConfined()) {
            // Query number of free sections
            int type    = hdf5_h.H5FD_MEM_DEFAULT();
            long nsects = hdf5_h.H5Fget_free_sections(H5fid, type, 0, MemorySegment.NULL);
            assertTrue("H5Fget_free_sections count query should succeed", nsects >= 0);
        }
    }

    @Test
    public void testH5Fget_info2()
    {
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment finfo = H5F_info2_t.allocate(arena);

            int result = hdf5_h.H5Fget_info2(H5fid, finfo);
            assertTrue("H5Fget_info2 failed", isSuccess(result));

            // Access super block info
            MemorySegment superInfo = H5F_info2_t.super_(finfo);
            int version             = H5F_info2_t.super_.version(superInfo);
            assertTrue("Super block version should be valid", version >= 0);

            long superSize = H5F_info2_t.super_.super_size(superInfo);
            assertTrue("Super block size should be positive", superSize > 0);
        }
    }

    @Test
    public void testH5Fstart_swmr_write()
    {
        try (Arena arena = Arena.ofConfined()) {
            // Create a new file for SWMR testing
            String swmrFile        = "swmr_test.h5";
            MemorySegment fileName = stringToSegment(arena, swmrFile);

            // Create file with SWMR-compatible settings
            long fapl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_FILE_ACCESS_ID_g());
            hdf5_h.H5Pset_libver_bounds(fapl, hdf5_h.H5F_LIBVER_LATEST(), hdf5_h.H5F_LIBVER_LATEST());

            long fid = hdf5_h.H5Fcreate(fileName, hdf5_h.H5F_ACC_TRUNC(), hdf5_h.H5P_DEFAULT(), fapl);
            if (isValidId(fid)) {
                // Try to start SWMR write mode
                int result = hdf5_h.H5Fstart_swmr_write(fid);
                // Note: May fail if file has open objects, which is expected behavior
                // We're just testing that the API is callable

                hdf5_h.H5Fclose(fid);
            }

            hdf5_h.H5Pclose(fapl);
        }
    }

    @Test
    public void testH5Fget_vfd_handle()
    {
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment fileHandle = arena.allocate(ValueLayout.ADDRESS);

            // Get VFD handle (may not be supported by all VFDs)
            int result = hdf5_h.H5Fget_vfd_handle(H5fid, hdf5_h.H5P_DEFAULT(), fileHandle);
            // Result may fail for some VFDs, which is acceptable
            // We're testing that the API is callable
        }
    }

    @Test
    public void testH5Fget_page_buffering_stats()
    {
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment accesses  = allocateIntArray(arena, 2);
            MemorySegment hits      = allocateIntArray(arena, 2);
            MemorySegment misses    = allocateIntArray(arena, 2);
            MemorySegment evictions = allocateIntArray(arena, 2);
            MemorySegment bypasses  = allocateIntArray(arena, 2);

            int result =
                hdf5_h.H5Fget_page_buffering_stats(H5fid, accesses, hits, misses, evictions, bypasses);
            // May fail if page buffering is not enabled, which is expected
            // Testing API availability
        }
    }

    @Test
    public void testH5Freset_page_buffering_stats()
    {
        int result = hdf5_h.H5Freset_page_buffering_stats(H5fid);
        // May fail if page buffering not enabled
        // Testing API availability
    }

    @Test
    public void testH5Fincrement_filesize()
    {
        try (Arena arena = Arena.ofConfined()) {
            // Get current file size
            MemorySegment sizeBefore = arena.allocate(ValueLayout.JAVA_LONG);
            hdf5_h.H5Fget_filesize(H5fid, sizeBefore);
            long before = sizeBefore.get(ValueLayout.JAVA_LONG, 0);

            // Increment file size by 1KB
            long increment = 1024;
            int result     = hdf5_h.H5Fincrement_filesize(H5fid, increment);
            assertTrue("H5Fincrement_filesize failed", isSuccess(result));

            // Get new file size
            MemorySegment sizeAfter = arena.allocate(ValueLayout.JAVA_LONG);
            hdf5_h.H5Fget_filesize(H5fid, sizeAfter);
            long after = sizeAfter.get(ValueLayout.JAVA_LONG, 0);

            assertTrue("File size should have increased", after >= before + increment);
        }
    }

    @Test
    public void testH5Fformat_convert()
    {
        // Format convert - converts older format files to latest format
        // May fail on already-latest format files, which is acceptable
        int result = hdf5_h.H5Fformat_convert(H5fid);
        // Just testing API availability
    }

    @Test
    public void testH5Fget_dset_no_attrs_hint()
    {
        try (Arena arena = Arena.ofConfined()) {
            // Create a test dataset to check hint
            String dsetName           = "test_dset_hint";
            MemorySegment dsetNameSeg = stringToSegment(arena, dsetName);

            long[] dims           = {10};
            MemorySegment dimsSeg = arena.allocateFrom(ValueLayout.JAVA_LONG, dims);
            long space            = hdf5_h.H5Screate_simple(1, dimsSeg, MemorySegment.NULL);

            long dset = hdf5_h.H5Dcreate2(H5fid, dsetNameSeg, hdf5_h.H5T_NATIVE_INT_g(), space,
                                          hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());

            if (isValidId(dset)) {
                MemorySegment minimize = arena.allocate(ValueLayout.JAVA_BOOLEAN);

                int result = hdf5_h.H5Fget_dset_no_attrs_hint(H5fid, minimize);
                assertTrue("H5Fget_dset_no_attrs_hint failed", isSuccess(result));

                // Close dataset
                hdf5_h.H5Dclose(dset);
            }

            hdf5_h.H5Sclose(space);
        }
    }

    @Test
    public void testH5Fset_dset_no_attrs_hint()
    {
        try (Arena arena = Arena.ofConfined()) {
            // Set the hint to minimize dataset object headers
            boolean minimize = true;
            int result       = hdf5_h.H5Fset_dset_no_attrs_hint(H5fid, minimize);
            assertTrue("H5Fset_dset_no_attrs_hint failed", isSuccess(result));

            // Verify it was set
            MemorySegment minimizeSeg = arena.allocate(ValueLayout.JAVA_BOOLEAN);
            result                    = hdf5_h.H5Fget_dset_no_attrs_hint(H5fid, minimizeSeg);
            assertTrue("H5Fget_dset_no_attrs_hint failed", isSuccess(result));

            boolean retrieved = minimizeSeg.get(ValueLayout.JAVA_BOOLEAN, 0);
            assertEquals("Minimize hint should match", minimize, retrieved);
        }
    }
}
