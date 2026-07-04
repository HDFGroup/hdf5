/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
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

import org.hdfgroup.javahdf5.H5O_info2_t;
import org.hdfgroup.javahdf5.H5O_native_info_t;
import org.hdfgroup.javahdf5.H5O_token_t;
import org.hdfgroup.javahdf5.hdf5_h;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

/**
 * TestH5Offm - FFM-based tests for HDF5 Object operations.
 * Tests the H5O* API using Foreign Function & Memory (FFM) bindings.
 */
public class TestH5Offm {
    private static final String H5_FILE  = "testO.h5";
    private static final String H5_FILE2 = "testO2.h5";
    private static final int DIM_X       = 4;
    private static final int DIM_Y       = 6;
    private static final int RANK        = 2;

    @Rule
    public TestName testname = new TestName();

    long H5fid  = hdf5_h.H5I_INVALID_HID();
    long H5fid2 = hdf5_h.H5I_INVALID_HID();
    long H5did  = hdf5_h.H5I_INVALID_HID();
    long H5gid  = hdf5_h.H5I_INVALID_HID();
    long H5sid  = hdf5_h.H5I_INVALID_HID();

    @Before
    public void createH5file() throws Exception
    {
        try (Arena arena = Arena.ofConfined()) {
            // Create primary file
            MemorySegment fileName = stringToSegment(arena, H5_FILE);
            H5fid                  = hdf5_h.H5Fcreate(fileName, hdf5_h.H5F_ACC_TRUNC(), hdf5_h.H5P_DEFAULT(),
                                                      hdf5_h.H5P_DEFAULT());
            assertTrue("H5Fcreate failed", isValidId(H5fid));

            // Create dataspace
            long[] dims               = {DIM_X, DIM_Y};
            MemorySegment dimsSegment = allocateLongArray(arena, RANK);
            copyToSegment(dimsSegment, dims);
            H5sid = hdf5_h.H5Screate_simple(RANK, dimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple failed", isValidId(H5sid));

            // Create dataset
            MemorySegment dsetName = stringToSegment(arena, "dset");
            H5did = hdf5_h.H5Dcreate2(H5fid, dsetName, hdf5_h.H5T_NATIVE_INT_g(), H5sid, hdf5_h.H5P_DEFAULT(),
                                      hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());
            assertTrue("H5Dcreate2 failed", isValidId(H5did));

            // Create group
            MemorySegment groupName = stringToSegment(arena, "group");
            H5gid = hdf5_h.H5Gcreate2(H5fid, groupName, hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT(),
                                      hdf5_h.H5P_DEFAULT());
            assertTrue("H5Gcreate2 failed", isValidId(H5gid));
        }
    }

    @After
    public void deleteH5file() throws Exception
    {
        closeQuietly(H5gid, hdf5_h::H5Gclose);
        closeQuietly(H5did, hdf5_h::H5Dclose);
        closeQuietly(H5sid, hdf5_h::H5Sclose);
        closeQuietly(H5fid, hdf5_h::H5Fclose);
        closeQuietly(H5fid2, hdf5_h::H5Fclose);

        H5gid  = hdf5_h.H5I_INVALID_HID();
        H5did  = hdf5_h.H5I_INVALID_HID();
        H5sid  = hdf5_h.H5I_INVALID_HID();
        H5fid  = hdf5_h.H5I_INVALID_HID();
        H5fid2 = hdf5_h.H5I_INVALID_HID();
    }

    static
    {
        try {
            System.loadLibrary("hdf5");
            hdf5_h.H5open();
        }
        catch (UnsatisfiedLinkError e) {
            System.err.println("Failed to load HDF5 library: " + e.getMessage());
        }
    }

    /**
     * Test H5Oopen and H5Oclose - Basic object open/close operations
     */
    @Test
    public void testH5Oopen_close()
    {
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment dsetName = stringToSegment(arena, "dset");

            // Open dataset as object
            long oid = hdf5_h.H5Oopen(H5fid, dsetName, hdf5_h.H5P_DEFAULT());
            assertTrue("H5Oopen should return valid ID", isValidId(oid));

            // Close object
            int ret = hdf5_h.H5Oclose(oid);
            assertTrue("H5Oclose should succeed", isSuccess(ret));
        }
    }

    /**
     * Test H5Oopen with group object
     */
    @Test
    public void testH5Oopen_group()
    {
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment groupName = stringToSegment(arena, "group");

            // Open group as object
            long oid = hdf5_h.H5Oopen(H5fid, groupName, hdf5_h.H5P_DEFAULT());
            assertTrue("H5Oopen should return valid ID for group", isValidId(oid));

            int ret = hdf5_h.H5Oclose(oid);
            assertTrue("H5Oclose should succeed", isSuccess(ret));
        }
    }

    /**
     * Test H5Oget_info3 - Get object information
     */
    @Test
    public void testH5Oget_info()
    {
        try (Arena arena = Arena.ofConfined()) {
            // Allocate H5O_info2_t structure
            MemorySegment oinfo = H5O_info2_t.allocate(arena);

            // Get info for dataset
            int ret = hdf5_h.H5Oget_info3(H5did, oinfo, hdf5_h.H5O_INFO_ALL());
            assertTrue("H5Oget_info3 should succeed", isSuccess(ret));

            // Verify we got valid information
            int type = H5O_info2_t.type(oinfo);
            assertTrue("Object type should be valid", type >= 0);
            assertEquals("Should be dataset type", hdf5_h.H5O_TYPE_DATASET(), type);
        }
    }

    /**
     * Test H5Oget_info3 on group object
     */
    @Test
    public void testH5Oget_info_group()
    {
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment oinfo = H5O_info2_t.allocate(arena);

            int ret = hdf5_h.H5Oget_info3(H5gid, oinfo, hdf5_h.H5O_INFO_ALL());
            assertTrue("H5Oget_info3 should succeed for group", isSuccess(ret));

            int type = H5O_info2_t.type(oinfo);
            assertEquals("Should be group type", hdf5_h.H5O_TYPE_GROUP(), type);
        }
    }

    /**
     * Test H5Oget_info_by_name3 - Get object info by name
     */
    @Test
    public void testH5Oget_info_by_name()
    {
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment dsetName = stringToSegment(arena, "dset");
            MemorySegment oinfo    = H5O_info2_t.allocate(arena);

            int ret = hdf5_h.H5Oget_info_by_name3(H5fid, dsetName, oinfo, hdf5_h.H5O_INFO_ALL(),
                                                  hdf5_h.H5P_DEFAULT());
            assertTrue("H5Oget_info_by_name3 should succeed", isSuccess(ret));

            int type = H5O_info2_t.type(oinfo);
            assertEquals("Should be dataset type", hdf5_h.H5O_TYPE_DATASET(), type);
        }
    }

    /**
     * Test H5Oget_info_by_idx3 - Get object info by index
     */
    @Test
    public void testH5Oget_info_by_idx()
    {
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment rootName = stringToSegment(arena, ".");
            MemorySegment oinfo    = H5O_info2_t.allocate(arena);

            // Get info for first object in root group (by creation order)
            int ret =
                hdf5_h.H5Oget_info_by_idx3(H5fid, rootName, hdf5_h.H5_INDEX_NAME(), hdf5_h.H5_ITER_INC(), 0,
                                           oinfo, hdf5_h.H5O_INFO_ALL(), hdf5_h.H5P_DEFAULT());
            assertTrue("H5Oget_info_by_idx3 should succeed", isSuccess(ret));

            int type = H5O_info2_t.type(oinfo);
            assertTrue("Object type should be valid", type >= 0);
        }
    }

    /**
     * Test H5Oexists_by_name - Check if object exists
     */
    @Test
    public void testH5Oexists_by_name()
    {
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment dsetName = stringToSegment(arena, "dset");

            // Check if dataset exists
            int ret = hdf5_h.H5Oexists_by_name(H5fid, dsetName, hdf5_h.H5P_DEFAULT());
            assertTrue("H5Oexists_by_name should return true for existing object", ret > 0);

            // Check non-existent object
            MemorySegment noName = stringToSegment(arena, "nonexistent");
            ret                  = hdf5_h.H5Oexists_by_name(H5fid, noName, hdf5_h.H5P_DEFAULT());
            assertFalse("H5Oexists_by_name should return false for non-existent object", ret > 0);
        }
    }

    /**
     * Test H5Olink - Create hard link to object
     */
    @Test
    public void testH5Olink()
    {
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment linkName = stringToSegment(arena, "dset_link");

            // Create hard link to dataset
            int ret = hdf5_h.H5Olink(H5did, H5fid, linkName, hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());
            assertTrue("H5Olink should succeed", isSuccess(ret));

            // Verify link exists
            long oid = hdf5_h.H5Oopen(H5fid, linkName, hdf5_h.H5P_DEFAULT());
            assertTrue("Should be able to open linked object", isValidId(oid));
            hdf5_h.H5Oclose(oid);
        }
    }

    /**
     * Test H5Ocopy - Copy object to new location
     */
    @Test
    public void testH5Ocopy()
    {
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment srcName = stringToSegment(arena, "dset");
            MemorySegment dstName = stringToSegment(arena, "dset_copy");

            // Copy dataset within same file
            int ret =
                hdf5_h.H5Ocopy(H5fid, srcName, H5fid, dstName, hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());
            assertTrue("H5Ocopy should succeed", isSuccess(ret));

            // Verify copy exists
            long oid = hdf5_h.H5Oopen(H5fid, dstName, hdf5_h.H5P_DEFAULT());
            assertTrue("Should be able to open copied object", isValidId(oid));
            hdf5_h.H5Oclose(oid);
        }
    }

    /**
     * Test H5Ocopy across files
     */
    @Test
    public void testH5Ocopy_across_files()
    {
        try (Arena arena = Arena.ofConfined()) {
            // Create second file
            MemorySegment fileName2 = stringToSegment(arena, H5_FILE2);
            H5fid2 = hdf5_h.H5Fcreate(fileName2, hdf5_h.H5F_ACC_TRUNC(), hdf5_h.H5P_DEFAULT(),
                                      hdf5_h.H5P_DEFAULT());
            assertTrue("H5Fcreate for second file should succeed", isValidId(H5fid2));

            MemorySegment srcName = stringToSegment(arena, "dset");
            MemorySegment dstName = stringToSegment(arena, "dset_from_file1");

            // Copy dataset to different file
            int ret =
                hdf5_h.H5Ocopy(H5fid, srcName, H5fid2, dstName, hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());
            assertTrue("H5Ocopy across files should succeed", isSuccess(ret));

            // Verify copy exists in destination file
            long oid = hdf5_h.H5Oopen(H5fid2, dstName, hdf5_h.H5P_DEFAULT());
            assertTrue("Should be able to open copied object in destination file", isValidId(oid));
            hdf5_h.H5Oclose(oid);
        }
    }

    /**
     * Test H5Oget_comment and H5Oset_comment - Object comments
     */
    @Test
    public void testH5O_comment()
    {
        try (Arena arena = Arena.ofConfined()) {
            String comment           = "This is a test dataset";
            MemorySegment commentSeg = stringToSegment(arena, comment);

            // Set comment on dataset
            int ret = hdf5_h.H5Oset_comment(H5did, commentSeg);
            assertTrue("H5Oset_comment should succeed", isSuccess(ret));

            // Get comment size
            long commentSize = hdf5_h.H5Oget_comment(H5did, MemorySegment.NULL, 0);
            assertTrue("H5Oget_comment should return positive size", commentSize > 0);

            // Allocate buffer and get comment
            MemorySegment buffer = arena.allocate(commentSize + 1);
            long actualSize      = hdf5_h.H5Oget_comment(H5did, buffer, commentSize + 1);
            assertTrue("H5Oget_comment should return actual size", actualSize > 0);

            String retrievedComment = segmentToString(buffer);
            assertEquals("Retrieved comment should match", comment, retrievedComment);
        }
    }

    /**
     * Test H5Oget_comment_by_name and H5Oset_comment_by_name
     */
    @Test
    public void testH5O_comment_by_name()
    {
        try (Arena arena = Arena.ofConfined()) {
            String comment           = "Dataset comment by name";
            MemorySegment commentSeg = stringToSegment(arena, comment);
            MemorySegment dsetName   = stringToSegment(arena, "dset");

            // Set comment by name
            int ret = hdf5_h.H5Oset_comment_by_name(H5fid, dsetName, commentSeg, hdf5_h.H5P_DEFAULT());
            assertTrue("H5Oset_comment_by_name should succeed", isSuccess(ret));

            // Get comment size by name
            long commentSize =
                hdf5_h.H5Oget_comment_by_name(H5fid, dsetName, MemorySegment.NULL, 0, hdf5_h.H5P_DEFAULT());
            assertTrue("H5Oget_comment_by_name should return positive size", commentSize > 0);

            // Get comment by name
            MemorySegment buffer = arena.allocate(commentSize + 1);
            long actualSize =
                hdf5_h.H5Oget_comment_by_name(H5fid, dsetName, buffer, commentSize + 1, hdf5_h.H5P_DEFAULT());
            assertTrue("Should get actual comment size", actualSize > 0);

            String retrievedComment = segmentToString(buffer);
            assertEquals("Retrieved comment should match", comment, retrievedComment);
        }
    }

    /**
     * Test H5Oincr_refcount and H5Odecr_refcount - Reference counting
     */
    @Test
    public void testH5O_refcount()
    {
        try (Arena arena = Arena.ofConfined()) {
            // Increment reference count
            int ret = hdf5_h.H5Oincr_refcount(H5did);
            assertTrue("H5Oincr_refcount should succeed", isSuccess(ret));

            // Decrement reference count
            ret = hdf5_h.H5Odecr_refcount(H5did);
            assertTrue("H5Odecr_refcount should succeed", isSuccess(ret));
        }
    }

    /**
     * Test H5Oflush - Flush object metadata
     */
    @Test
    public void testH5Oflush()
    {
        int ret = hdf5_h.H5Oflush(H5did);
        assertTrue("H5Oflush should succeed", isSuccess(ret));
    }

    /**
     * Test H5Oget_native_info - Get native object information
     */
    @Test
    public void testH5Oget_native_info()
    {
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment ninfo = H5O_native_info_t.allocate(arena);

            int ret = hdf5_h.H5Oget_native_info(H5did, ninfo, hdf5_h.H5O_NATIVE_INFO_ALL());
            assertTrue("H5Oget_native_info should succeed", isSuccess(ret));

            // Verify we got valid information
            // Header info should have valid values
            MemorySegment hdr = H5O_native_info_t.hdr(ninfo);
            assertNotNull("Header info should not be null", hdr);
        }
    }

    /**
     * Test H5Oget_native_info_by_name
     */
    @Test
    public void testH5Oget_native_info_by_name()
    {
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment dsetName = stringToSegment(arena, "dset");
            MemorySegment ninfo    = H5O_native_info_t.allocate(arena);

            int ret = hdf5_h.H5Oget_native_info_by_name(H5fid, dsetName, ninfo, hdf5_h.H5O_NATIVE_INFO_ALL(),
                                                        hdf5_h.H5P_DEFAULT());
            assertTrue("H5Oget_native_info_by_name should succeed", isSuccess(ret));

            MemorySegment hdr = H5O_native_info_t.hdr(ninfo);
            assertNotNull("Header info should not be null", hdr);
        }
    }

    /**
     * Test H5Oopen_by_idx - Open object by index
     */
    @Test
    public void testH5Oopen_by_idx()
    {
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment rootName = stringToSegment(arena, ".");

            // Open first object by name index
            long oid = hdf5_h.H5Oopen_by_idx(H5fid, rootName, hdf5_h.H5_INDEX_NAME(), hdf5_h.H5_ITER_INC(), 0,
                                             hdf5_h.H5P_DEFAULT());
            assertTrue("H5Oopen_by_idx should return valid ID", isValidId(oid));

            int ret = hdf5_h.H5Oclose(oid);
            assertTrue("H5Oclose should succeed", isSuccess(ret));
        }
    }

    /**
     * Test H5Oopen_by_token - Open object by token
     */
    @Test
    public void testH5Oopen_by_token()
    {
        try (Arena arena = Arena.ofConfined()) {
            // Get token for dataset
            MemorySegment oinfo = H5O_info2_t.allocate(arena);
            int ret             = hdf5_h.H5Oget_info3(H5did, oinfo, hdf5_h.H5O_INFO_BASIC());
            assertTrue("H5Oget_info3 should succeed", isSuccess(ret));

            // Get the token
            MemorySegment token = H5O_info2_t.token(oinfo);
            assertNotNull("Token should not be null", token);

            // Open object by token
            long oid = hdf5_h.H5Oopen_by_token(H5fid, token);
            assertTrue("H5Oopen_by_token should return valid ID", isValidId(oid));

            ret = hdf5_h.H5Oclose(oid);
            assertTrue("H5Oclose should succeed", isSuccess(ret));
        }
    }

    /**
     * Test H5Oare_mdc_flushes_disabled, H5Odisable_mdc_flushes, H5Oenable_mdc_flushes
     */
    @Test
    public void testH5O_mdc_flushes()
    {
        try (Arena arena = Arena.ofConfined()) {
            // Check initial state
            MemorySegment areDisabled = arena.allocate(ValueLayout.JAVA_BYTE);
            int ret                   = hdf5_h.H5Oare_mdc_flushes_disabled(H5did, areDisabled);
            assertTrue("H5Oare_mdc_flushes_disabled should succeed", isSuccess(ret));

            // Disable flushes
            ret = hdf5_h.H5Odisable_mdc_flushes(H5did);
            assertTrue("H5Odisable_mdc_flushes should succeed", isSuccess(ret));

            // Check they are disabled
            ret = hdf5_h.H5Oare_mdc_flushes_disabled(H5did, areDisabled);
            assertTrue("Should be able to check flush state", isSuccess(ret));
            assertTrue("Flushes should be disabled", areDisabled.get(ValueLayout.JAVA_BYTE, 0) > 0);

            // Re-enable flushes
            ret = hdf5_h.H5Oenable_mdc_flushes(H5did);
            assertTrue("H5Oenable_mdc_flushes should succeed", isSuccess(ret));

            // Check they are enabled
            ret = hdf5_h.H5Oare_mdc_flushes_disabled(H5did, areDisabled);
            assertTrue("Should be able to check flush state", isSuccess(ret));
            assertFalse("Flushes should be enabled", areDisabled.get(ValueLayout.JAVA_BYTE, 0) > 0);
        }
    }
}
