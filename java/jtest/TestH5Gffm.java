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

import org.hdfgroup.javahdf5.hdf5_h;
import org.hdfgroup.javahdf5.hdf5_h_1;
import org.hdfgroup.javahdf5.H5G_info_t;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

/**
 * FFM-only tests for HDF5 Group (H5G) operations.
 *
 * This test class uses direct FFM bindings without the hdf.hdf5lib wrapper layer.
 */
public class TestH5Gffm {
    @Rule
    public TestName testname = new TestName();

    private static final String H5_FILE = "test_H5Gffm.h5";

    long H5fid = hdf5_h.H5I_INVALID_HID();
    long H5gid = hdf5_h.H5I_INVALID_HID();

    @Before
    public void createH5file()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create file
            MemorySegment filename = stringToSegment(arena, H5_FILE);
            H5fid = hdf5_h_1.H5Fcreate(filename, hdf5_h.H5F_ACC_TRUNC(), hdf5_h.H5P_DEFAULT(),
                                       hdf5_h.H5P_DEFAULT());
            assertTrue("H5Fcreate failed", isValidId(H5fid));

            // Create root group for testing
            MemorySegment groupname = stringToSegment(arena, "TestGroup");
            H5gid = hdf5_h_1.H5Gcreate2(H5fid, groupname, hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT(),
                                        hdf5_h.H5P_DEFAULT());
            assertTrue("H5Gcreate2 failed", isValidId(H5gid));
        }
    }

    @After
    public void deleteH5file()
    {
        if (isValidId(H5gid)) {
            closeQuietly(H5gid, hdf5_h_1::H5Gclose);
            H5gid = hdf5_h.H5I_INVALID_HID();
        }
        if (isValidId(H5fid)) {
            closeQuietly(H5fid, hdf5_h_1::H5Fclose);
            H5fid = hdf5_h.H5I_INVALID_HID();
        }
        System.out.println();
    }

    // ============================================================================
    // Phase 1: Group Creation and Closing
    // ============================================================================

    @Test
    public void testH5Gcreate2_close()
    {
        try (Arena arena = Arena.ofConfined()) {
            // Create a group
            MemorySegment groupname = stringToSegment(arena, "Group1");
            long gid = hdf5_h_1.H5Gcreate2(H5fid, groupname, hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT(),
                                           hdf5_h.H5P_DEFAULT());
            assertTrue("H5Gcreate2 failed", isValidId(gid));

            // Verify it's a group
            int obj_type = hdf5_h_1.H5Iget_type(gid);
            assertEquals("Should be group type", hdf5_h.H5I_GROUP(), obj_type);

            // Close group
            int result = hdf5_h_1.H5Gclose(gid);
            assertTrue("H5Gclose failed", isSuccess(result));
        }
    }

    @Test
    public void testH5Gopen2()
    {
        try (Arena arena = Arena.ofConfined()) {
            // Create a group
            MemorySegment groupname = stringToSegment(arena, "GroupToOpen");
            long gid1 = hdf5_h_1.H5Gcreate2(H5fid, groupname, hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT(),
                                            hdf5_h.H5P_DEFAULT());
            assertTrue("H5Gcreate2 failed", isValidId(gid1));
            hdf5_h_1.H5Gclose(gid1);

            // Open the group
            long gid2 = hdf5_h_1.H5Gopen2(H5fid, groupname, hdf5_h.H5P_DEFAULT());
            assertTrue("H5Gopen2 failed", isValidId(gid2));

            // Verify it's a group
            int obj_type = hdf5_h_1.H5Iget_type(gid2);
            assertEquals("Should be group type", hdf5_h.H5I_GROUP(), obj_type);

            hdf5_h_1.H5Gclose(gid2);
        }
    }

    @Test
    public void testH5Gcreate_anon()
    {
        try (Arena arena = Arena.ofConfined()) {
            // Create anonymous group
            long gid = hdf5_h_1.H5Gcreate_anon(H5fid, hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());
            assertTrue("H5Gcreate_anon failed", isValidId(gid));

            // Verify it's a group
            int obj_type = hdf5_h_1.H5Iget_type(gid);
            assertEquals("Should be group type", hdf5_h.H5I_GROUP(), obj_type);

            // Link it to a name
            MemorySegment linkname = stringToSegment(arena, "AnonGroup");
            int result = hdf5_h_1.H5Olink(gid, H5fid, linkname, hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());
            assertTrue("H5Olink failed", isSuccess(result));

            hdf5_h_1.H5Gclose(gid);
        }
    }

    // ============================================================================
    // Phase 2: Group Information
    // ============================================================================

    @Test
    public void testH5Gget_info()
    {
        try (Arena arena = Arena.ofConfined()) {
            // Create subgroups
            for (int i = 0; i < 3; i++) {
                MemorySegment subname = stringToSegment(arena, "SubGroup" + i);
                long sub_gid = hdf5_h_1.H5Gcreate2(H5gid, subname, hdf5_h.H5P_DEFAULT(),
                                                   hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());
                assertTrue("H5Gcreate2 subgroup failed", isValidId(sub_gid));
                hdf5_h_1.H5Gclose(sub_gid);
            }

            // Get group info
            MemorySegment ginfo = H5G_info_t.allocate(arena);
            int result          = hdf5_h_1.H5Gget_info(H5gid, ginfo);
            assertTrue("H5Gget_info failed", isSuccess(result));

            // Verify storage type
            int storage_type = H5G_info_t.storage_type(ginfo);
            assertTrue("Storage type should be valid", storage_type >= 0);

            // Verify link count
            long nlinks = H5G_info_t.nlinks(ginfo);
            assertEquals("Should have 3 links", 3L, nlinks);
        }
    }

    @Test
    public void testH5Gget_info_by_name()
    {
        try (Arena arena = Arena.ofConfined()) {
            // Create a subgroup
            MemorySegment subname = stringToSegment(arena, "InfoTestGroup");
            long sub_gid = hdf5_h_1.H5Gcreate2(H5fid, subname, hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT(),
                                               hdf5_h.H5P_DEFAULT());
            assertTrue("H5Gcreate2 failed", isValidId(sub_gid));
            hdf5_h_1.H5Gclose(sub_gid);

            // Get info by name from file
            MemorySegment ginfo = H5G_info_t.allocate(arena);
            int result = hdf5_h_1.H5Gget_info_by_name(H5fid, subname, ginfo, hdf5_h.H5P_DEFAULT());
            assertTrue("H5Gget_info_by_name failed", isSuccess(result));

            // Verify storage type
            int storage_type = H5G_info_t.storage_type(ginfo);
            assertTrue("Storage type should be valid", storage_type >= 0);
        }
    }

    @Test
    public void testH5Gget_info_by_idx()
    {
        try (Arena arena = Arena.ofConfined()) {
            // Create multiple groups
            for (int i = 0; i < 3; i++) {
                MemorySegment subname = stringToSegment(arena, "IdxGroup" + i);
                long sub_gid = hdf5_h_1.H5Gcreate2(H5fid, subname, hdf5_h.H5P_DEFAULT(),
                                                   hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());
                assertTrue("H5Gcreate2 failed", isValidId(sub_gid));
                hdf5_h_1.H5Gclose(sub_gid);
            }

            // Get info for group at index 1
            MemorySegment ginfo = H5G_info_t.allocate(arena);
            MemorySegment dotname = stringToSegment(arena, ".");
            int result = hdf5_h_1.H5Gget_info_by_idx(H5fid, dotname, hdf5_h.H5_INDEX_NAME(),
                                                     hdf5_h.H5_ITER_INC(), 1, ginfo, hdf5_h.H5P_DEFAULT());
            assertTrue("H5Gget_info_by_idx failed", isSuccess(result));

            // Verify storage type is valid
            int storage_type = H5G_info_t.storage_type(ginfo);
            assertTrue("Storage type should be valid", storage_type >= 0);
        }
    }

    // ============================================================================
    // Phase 3: Group Property List
    // ============================================================================

    @Test
    public void testH5Gget_create_plist()
    {
        try (Arena arena = Arena.ofConfined()) {
            // Create group with default GCPL
            MemorySegment groupname = stringToSegment(arena, "GroupWithGCPL");
            long gid = hdf5_h_1.H5Gcreate2(H5fid, groupname, hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT(),
                                           hdf5_h.H5P_DEFAULT());
            assertTrue("H5Gcreate2 failed", isValidId(gid));

            // Get GCPL back
            long retrieved_gcpl = hdf5_h_1.H5Gget_create_plist(gid);
            assertTrue("H5Gget_create_plist failed", isValidId(retrieved_gcpl));

            // Verify it's a valid property list
            int plist_class = hdf5_h_1.H5Iget_type(retrieved_gcpl);
            assertEquals("Should be property list type", hdf5_h.H5I_GENPROP_LST(), plist_class);

            // Clean up
            closeQuietly(retrieved_gcpl, hdf5_h_1::H5Dclose); // Use Dclose as generic close
            hdf5_h_1.H5Gclose(gid);
        }
    }

    // ============================================================================
    // Phase 4: Group Flush and Refresh
    // ============================================================================

    @Test
    public void testH5Gflush()
    {
        try (Arena arena = Arena.ofConfined()) {
            // Create a subgroup
            MemorySegment subname = stringToSegment(arena, "FlushGroup");
            long gid = hdf5_h_1.H5Gcreate2(H5gid, subname, hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT(),
                                           hdf5_h.H5P_DEFAULT());
            assertTrue("H5Gcreate2 failed", isValidId(gid));

            // Flush group
            int result = hdf5_h_1.H5Gflush(gid);
            assertTrue("H5Gflush failed", isSuccess(result));

            hdf5_h_1.H5Gclose(gid);
        }
    }

    @Test
    public void testH5Grefresh()
    {
        try (Arena arena = Arena.ofConfined()) {
            // Create a subgroup
            MemorySegment subname = stringToSegment(arena, "RefreshGroup");
            long gid = hdf5_h_1.H5Gcreate2(H5gid, subname, hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT(),
                                           hdf5_h.H5P_DEFAULT());
            assertTrue("H5Gcreate2 failed", isValidId(gid));

            // Flush first
            int result = hdf5_h_1.H5Gflush(gid);
            assertTrue("H5Gflush failed", isSuccess(result));

            // Refresh group
            result = hdf5_h_1.H5Grefresh(gid);
            assertTrue("H5Grefresh failed", isSuccess(result));

            hdf5_h_1.H5Gclose(gid);
        }
    }

    // ============================================================================
    // Phase 5: Comprehensive Workflow
    // ============================================================================

    @Test
    public void testH5G_complete_workflow()
    {
        try (Arena arena = Arena.ofConfined()) {
            // 1. Create group
            MemorySegment groupname = stringToSegment(arena, "WorkflowGroup");
            long gid = hdf5_h_1.H5Gcreate2(H5fid, groupname, hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT(),
                                           hdf5_h.H5P_DEFAULT());
            assertTrue("Create group failed", isValidId(gid));

            // 2. Create subgroups
            for (int i = 0; i < 3; i++) {
                MemorySegment subname = stringToSegment(arena, "Sub" + i);
                long sub_gid = hdf5_h_1.H5Gcreate2(gid, subname, hdf5_h.H5P_DEFAULT(),
                                                   hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());
                assertTrue("Create subgroup failed", isValidId(sub_gid));
                hdf5_h_1.H5Gclose(sub_gid);
            }

            // 3. Get group info
            MemorySegment ginfo = H5G_info_t.allocate(arena);
            int result          = hdf5_h_1.H5Gget_info(gid, ginfo);
            assertTrue("Get group info failed", isSuccess(result));

            long nlinks = H5G_info_t.nlinks(ginfo);
            assertEquals("Should have 3 links", 3L, nlinks);

            // 4. Get GCPL
            long retrieved_gcpl = hdf5_h_1.H5Gget_create_plist(gid);
            assertTrue("Get GCPL failed", isValidId(retrieved_gcpl));
            closeQuietly(retrieved_gcpl, hdf5_h_1::H5Dclose);

            // 5. Flush
            result = hdf5_h_1.H5Gflush(gid);
            assertTrue("Flush failed", isSuccess(result));

            // 6. Close and reopen
            hdf5_h_1.H5Gclose(gid);
            gid = hdf5_h_1.H5Gopen2(H5fid, groupname, hdf5_h.H5P_DEFAULT());
            assertTrue("Reopen group failed", isValidId(gid));

            // 7. Verify info still correct
            result = hdf5_h_1.H5Gget_info(gid, ginfo);
            assertTrue("Get info after reopen failed", isSuccess(result));

            nlinks = H5G_info_t.nlinks(ginfo);
            assertEquals("Should still have 3 links", 3L, nlinks);

            // Clean up
            hdf5_h_1.H5Gclose(gid);
        }
    }
}
