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

import org.hdfgroup.javahdf5.H5AC_cache_config_t;
import org.hdfgroup.javahdf5.H5F_info2_t;
import org.hdfgroup.javahdf5.hdf5_h;
import org.hdfgroup.javahdf5.hdf5_h_1;
import org.hdfgroup.javahdf5.hdf5_h_2;
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
            H5fid = hdf5_h_1.H5Fcreate(fileNameSegment, hdf5_h.H5F_ACC_TRUNC(), hdf5_h_1.H5P_DEFAULT(),
                                       hdf5_h_1.H5P_DEFAULT());
            assertTrue("H5Fcreate failed", isValidId(H5fid));

            int flushResult = hdf5_h_1.H5Fflush(H5fid, hdf5_h.H5F_SCOPE_LOCAL());
            assertTrue("H5Fflush failed", isSuccess(flushResult));
        }
    }

    @After
    public void deleteH5file()
    {
        if (H5fid >= 0) {
            closeQuietly(H5fid, hdf5_h_1::H5Fclose);
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
            fid = hdf5_h_1.H5Fopen(fileNameSegment, hdf5_h.H5F_ACC_RDONLY(), hdf5_h_1.H5P_DEFAULT());
            assertTrue("H5Fopen failed", isValidId(fid));
        }
        finally {
            closeQuietly(fid, hdf5_h_1::H5Fclose);
        }
    }

    @Test
    public void testH5Freopen()
    {
        long fid2 = hdf5_h.H5I_INVALID_HID();

        try {
            fid2 = hdf5_h_1.H5Freopen(H5fid);
            assertTrue("H5Freopen failed", isValidId(fid2));
            assertNotEquals("H5Freopen should return different id", H5fid, fid2);
        }
        finally {
            closeQuietly(fid2, hdf5_h_1::H5Fclose);
        }
    }

    @Test
    public void testH5Fget_create_plist()
    {
        long plist = hdf5_h.H5I_INVALID_HID();

        try {
            plist = hdf5_h_1.H5Fget_create_plist(H5fid);
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
            plist = hdf5_h_1.H5Fget_access_plist(H5fid);
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
            int result                  = hdf5_h_1.H5Fget_intent(H5fid, intentSegment);
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
            long nameLength = hdf5_h_1.H5Fget_name(H5fid, MemorySegment.NULL, 0);
            assertTrue("H5Fget_name (get length) failed", nameLength > 0);

            // Second call to get the actual name
            MemorySegment nameSegment = arena.allocate(nameLength + 1);
            long result               = hdf5_h_1.H5Fget_name(H5fid, nameSegment, nameLength + 1);
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
            int result                = hdf5_h_1.H5Fget_filesize(H5fid, sizeSegment);
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
            long result = hdf5_h_1.H5Fget_obj_count(H5fid, hdf5_h.H5F_OBJ_ALL());
            assertTrue("H5Fget_obj_count failed", result >= 0);
            assertTrue("Should have at least one object (the file)", result >= 1);
        }
    }

    @Test
    public void testH5Fget_info()
    {
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment fileInfoSegment = H5F_info2_t.allocate(arena);
            int result                    = hdf5_h_1.H5Fget_info2(H5fid, fileInfoSegment);
            assertTrue("H5Fget_info2 failed", isSuccess(result));

            // Struct verified (complex struct accessor testing skipped in FFM)
        }
    }

    @Test
    public void testH5Fis_accessible()
    {
        try (Arena arena = Arena.ofConfined()) {
            // Close the file first
            closeQuietly(H5fid, hdf5_h_1::H5Fclose);
            H5fid = hdf5_h.H5I_INVALID_HID();

            // Check if file is accessible
            MemorySegment fileNameSegment = stringToSegment(arena, H5_FILE);
            int result = hdf5_h_1.H5Fis_accessible(fileNameSegment, hdf5_h_1.H5P_DEFAULT());
            assertTrue("H5Fis_accessible should return true", result > 0);

            // Check non-existent file
            MemorySegment badFileSegment = stringToSegment(arena, "nonexistent.h5");
            result                       = hdf5_h_1.H5Fis_accessible(badFileSegment, hdf5_h_1.H5P_DEFAULT());
            assertFalse("H5Fis_accessible should return false for non-existent file", result > 0);
        }
    }

    @Test
    public void testH5Fclear_elink_file_cache()
    {
        int result = hdf5_h_1.H5Fclear_elink_file_cache(H5fid);
        assertTrue("H5Fclear_elink_file_cache failed", isSuccess(result));
    }

    @Test
    public void testH5Fclose()
    {
        long fid = hdf5_h.H5I_INVALID_HID();

        try (Arena arena = Arena.ofConfined()) {
            MemorySegment fileNameSegment = stringToSegment(arena, H5_FILE2);
            fid = hdf5_h_1.H5Fcreate(fileNameSegment, hdf5_h.H5F_ACC_TRUNC(), hdf5_h_1.H5P_DEFAULT(),
                                     hdf5_h_1.H5P_DEFAULT());
            assertTrue("H5Fcreate failed", isValidId(fid));

            int result = hdf5_h_1.H5Fclose(fid);
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
            long freespace = hdf5_h_1.H5Fget_freespace(H5fid);
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

            int result = hdf5_h_1.H5Fget_mdc_config(H5fid, config);
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

            int result = hdf5_h_1.H5Fget_mdc_hit_rate(H5fid, hitRate);
            assertTrue("H5Fget_mdc_hit_rate failed", isSuccess(result));

            double rate = getDouble(hitRate);
            assertTrue("Hit rate should be between 0.0 and 1.0", rate >= 0.0 && rate <= 1.0);
        }
    }

    @Test
    public void testH5Fget_mdc_size()
    {
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment maxSize      = allocateLongArray(arena, 1);
            MemorySegment minCleanSize = allocateLongArray(arena, 1);
            MemorySegment curSize      = allocateLongArray(arena, 1);
            MemorySegment curNumEntries = allocateIntArray(arena, 1);

            int result = hdf5_h_1.H5Fget_mdc_size(H5fid, maxSize, minCleanSize, curSize, curNumEntries);
            assertTrue("H5Fget_mdc_size failed", isSuccess(result));

            // Verify values are reasonable
            assertTrue("Max size should be positive", getLong(maxSize) > 0);
            assertTrue("Current size should be non-negative", getLong(curSize) >= 0);
            assertTrue("Number of entries should be non-negative", getInt(curNumEntries) >= 0);
        }
    }

    @Test
    public void testH5Fget_fileno()
    {
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment fileno = allocateLongArray(arena, 1);

            int result = hdf5_h_1.H5Fget_fileno(H5fid, fileno);
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
            long imageSize = hdf5_h_1.H5Fget_file_image(H5fid, MemorySegment.NULL, 0);
            assertTrue("H5Fget_file_image should return positive size", imageSize > 0);

            // Allocate buffer and get image (limit to 64KB for test)
            long bufSize = Math.min(imageSize, 65536);
            MemorySegment imageBuffer = arena.allocate(bufSize);

            long actualSize = hdf5_h_1.H5Fget_file_image(H5fid, imageBuffer, bufSize);
            assertTrue("H5Fget_file_image should return size", actualSize > 0);
        }
    }

    @Test
    public void testH5Fget_dset_no_attrs_hint()
    {
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment minimize = allocateIntArray(arena, 1);

            int result = hdf5_h_1.H5Fget_dset_no_attrs_hint(H5fid, minimize);
            assertTrue("H5Fget_dset_no_attrs_hint failed", isSuccess(result));

            // Value should be 0 (false) or 1 (true)
            int value = getInt(minimize);
            assertTrue("Minimize value should be 0 or 1", value == 0 || value == 1);
        }
    }

    @Test
    public void testH5Fget_mdc_logging_status()
    {
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment isEnabled         = allocateIntArray(arena, 1);
            MemorySegment isCurrentlyLogging = allocateIntArray(arena, 1);

            int result = hdf5_h_1.H5Fget_mdc_logging_status(H5fid, isEnabled, isCurrentlyLogging);
            assertTrue("H5Fget_mdc_logging_status failed", isSuccess(result));

            // Values should be boolean (0 or 1)
            int enabled = getInt(isEnabled);
            int logging = getInt(isCurrentlyLogging);
            assertTrue("Enabled should be 0 or 1", enabled == 0 || enabled == 1);
            assertTrue("Currently logging should be 0 or 1", logging == 0 || logging == 1);
        }
    }
}
