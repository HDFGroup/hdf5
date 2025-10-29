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

import org.hdfgroup.javahdf5.H5L_info2_t;
import org.hdfgroup.javahdf5.hdf5_h;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

/**
 * FFM-only tests for HDF5 Link (H5L) operations.
 *
 * This test class uses direct FFM bindings without the hdf.hdf5lib wrapper layer.
 */
public class TestH5Lffm {
    @Rule
    public TestName testname = new TestName();

    private static final String H5_FILE     = "test_H5Lffm.h5";
    private static final String H5_FILE_EXT = "test_H5Lffm_ext.h5";

    long H5fid = hdf5_h.H5I_INVALID_HID();
    long H5gid = hdf5_h.H5I_INVALID_HID();
    long H5did = hdf5_h.H5I_INVALID_HID();

    @Before
    public void createH5file()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create file
            MemorySegment filename = stringToSegment(arena, H5_FILE);
            H5fid                  = hdf5_h.H5Fcreate(filename, hdf5_h.H5F_ACC_TRUNC(), hdf5_h.H5P_DEFAULT(),
                                                      hdf5_h.H5P_DEFAULT());
            assertTrue("H5Fcreate failed", isValidId(H5fid));

            // Create group
            MemorySegment groupname = stringToSegment(arena, "Group1");
            H5gid = hdf5_h.H5Gcreate2(H5fid, groupname, hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT(),
                                      hdf5_h.H5P_DEFAULT());
            assertTrue("H5Gcreate2 failed", isValidId(H5gid));

            // Create dataset
            long[] dims               = {10};
            MemorySegment dimsSegment = allocateLongArray(arena, 1);
            copyToSegment(dimsSegment, dims);

            long sid = hdf5_h.H5Screate_simple(1, dimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple failed", isValidId(sid));

            MemorySegment dsetname = stringToSegment(arena, "Dataset1");
            H5did = hdf5_h.H5Dcreate2(H5fid, dsetname, hdf5_h.H5T_NATIVE_INT_g(), sid, hdf5_h.H5P_DEFAULT(),
                                      hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());
            assertTrue("H5Dcreate2 failed", isValidId(H5did));

            hdf5_h.H5Sclose(sid);
        }
    }

    @After
    public void deleteH5file()
    {
        if (isValidId(H5did)) {
            closeQuietly(H5did, hdf5_h::H5Dclose);
            H5did = hdf5_h.H5I_INVALID_HID();
        }
        if (isValidId(H5gid)) {
            closeQuietly(H5gid, hdf5_h::H5Gclose);
            H5gid = hdf5_h.H5I_INVALID_HID();
        }
        if (isValidId(H5fid)) {
            closeQuietly(H5fid, hdf5_h::H5Fclose);
            H5fid = hdf5_h.H5I_INVALID_HID();
        }
        System.out.println();
    }

    // ============================================================================
    // Phase 1: Hard Link Operations
    // ============================================================================

    @Test
    public void testH5Lcreate_hard()
    {
        try (Arena arena = Arena.ofConfined()) {
            // Create hard link to existing dataset
            MemorySegment src_name  = stringToSegment(arena, "Dataset1");
            MemorySegment link_name = stringToSegment(arena, "HardLink1");

            int result = hdf5_h.H5Lcreate_hard(H5fid, src_name, H5fid, link_name, hdf5_h.H5P_DEFAULT(),
                                               hdf5_h.H5P_DEFAULT());
            assertTrue("H5Lcreate_hard failed", isSuccess(result));

            // Verify link exists
            result = hdf5_h.H5Lexists(H5fid, link_name, hdf5_h.H5P_DEFAULT());
            assertTrue("Link should exist", result > 0);

            // Open via hard link
            long did = hdf5_h.H5Dopen2(H5fid, link_name, hdf5_h.H5P_DEFAULT());
            assertTrue("H5Dopen2 via hard link failed", isValidId(did));

            hdf5_h.H5Dclose(did);
        }
    }

    @Test
    public void testH5Lcopy()
    {
        try (Arena arena = Arena.ofConfined()) {
            // Copy existing dataset link
            MemorySegment src_name  = stringToSegment(arena, "Dataset1");
            MemorySegment dest_name = stringToSegment(arena, "CopiedLink");

            int result =
                hdf5_h.H5Lcopy(H5fid, src_name, H5fid, dest_name, hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());
            assertTrue("H5Lcopy failed", isSuccess(result));

            // Verify copied link exists
            result = hdf5_h.H5Lexists(H5fid, dest_name, hdf5_h.H5P_DEFAULT());
            assertTrue("Copied link should exist", result > 0);
        }
    }

    @Test
    public void testH5Lmove()
    {
        try (Arena arena = Arena.ofConfined()) {
            // Create a link to move
            MemorySegment src_name   = stringToSegment(arena, "Dataset1");
            MemorySegment link_name  = stringToSegment(arena, "TempLink");
            MemorySegment moved_name = stringToSegment(arena, "MovedLink");

            // Create initial link
            int result = hdf5_h.H5Lcreate_hard(H5fid, src_name, H5fid, link_name, hdf5_h.H5P_DEFAULT(),
                                               hdf5_h.H5P_DEFAULT());
            assertTrue("H5Lcreate_hard failed", isSuccess(result));

            // Move link
            result = hdf5_h.H5Lmove(H5fid, link_name, H5fid, moved_name, hdf5_h.H5P_DEFAULT(),
                                    hdf5_h.H5P_DEFAULT());
            assertTrue("H5Lmove failed", isSuccess(result));

            // Verify old link doesn't exist
            result = hdf5_h.H5Lexists(H5fid, link_name, hdf5_h.H5P_DEFAULT());
            assertEquals("Old link should not exist", 0, result);

            // Verify new link exists
            result = hdf5_h.H5Lexists(H5fid, moved_name, hdf5_h.H5P_DEFAULT());
            assertTrue("Moved link should exist", result > 0);
        }
    }

    // ============================================================================
    // Phase 2: Soft Link Operations
    // ============================================================================

    @Test
    public void testH5Lcreate_soft()
    {
        try (Arena arena = Arena.ofConfined()) {
            // Create soft link to dataset
            MemorySegment target_path = stringToSegment(arena, "/Dataset1");
            MemorySegment link_name   = stringToSegment(arena, "SoftLink1");

            int result = hdf5_h.H5Lcreate_soft(target_path, H5fid, link_name, hdf5_h.H5P_DEFAULT(),
                                               hdf5_h.H5P_DEFAULT());
            assertTrue("H5Lcreate_soft failed", isSuccess(result));

            // Verify link exists
            result = hdf5_h.H5Lexists(H5fid, link_name, hdf5_h.H5P_DEFAULT());
            assertTrue("Soft link should exist", result > 0);

            // Get link value (use reasonable buffer size for soft link)
            long buf_size            = 256;
            MemorySegment val_buffer = arena.allocate(buf_size);
            result = hdf5_h.H5Lget_val(H5fid, link_name, val_buffer, buf_size, hdf5_h.H5P_DEFAULT());
            assertTrue("H5Lget_val failed", isSuccess(result));

            String link_target = val_buffer.getString(0);
            assertEquals("Link target should match", "/Dataset1", link_target);
        }
    }

    // ============================================================================
    // Phase 3: External Link Operations
    // ============================================================================

    @Test
    public void testH5Lcreate_external()
    {
        try (Arena arena = Arena.ofConfined()) {
            // Create external file
            MemorySegment ext_filename = stringToSegment(arena, H5_FILE_EXT);
            long ext_fid = hdf5_h.H5Fcreate(ext_filename, hdf5_h.H5F_ACC_TRUNC(), hdf5_h.H5P_DEFAULT(),
                                            hdf5_h.H5P_DEFAULT());
            assertTrue("H5Fcreate external failed", isValidId(ext_fid));

            // Create group in external file
            MemorySegment ext_groupname = stringToSegment(arena, "ExtGroup");
            long ext_gid                = hdf5_h.H5Gcreate2(ext_fid, ext_groupname, hdf5_h.H5P_DEFAULT(),
                                                            hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());
            assertTrue("H5Gcreate2 external failed", isValidId(ext_gid));

            hdf5_h.H5Gclose(ext_gid);
            hdf5_h.H5Fclose(ext_fid);

            // Create external link in main file
            MemorySegment obj_path  = stringToSegment(arena, "/ExtGroup");
            MemorySegment link_name = stringToSegment(arena, "ExternalLink");

            int result = hdf5_h.H5Lcreate_external(ext_filename, obj_path, H5fid, link_name,
                                                   hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());
            assertTrue("H5Lcreate_external failed", isSuccess(result));

            // Verify link exists
            result = hdf5_h.H5Lexists(H5fid, link_name, hdf5_h.H5P_DEFAULT());
            assertTrue("External link should exist", result > 0);
        }
    }

    @Test
    public void testH5Lunpack_elink_val()
    {
        try (Arena arena = Arena.ofConfined()) {
            // Create external link
            MemorySegment ext_filename = stringToSegment(arena, H5_FILE_EXT);
            MemorySegment obj_path     = stringToSegment(arena, "/SomeObject");
            MemorySegment link_name    = stringToSegment(arena, "ExternalLinkToUnpack");

            int result = hdf5_h.H5Lcreate_external(ext_filename, obj_path, H5fid, link_name,
                                                   hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());
            assertTrue("H5Lcreate_external failed", isSuccess(result));

            // Get link value (use reasonable buffer size)
            long buf_size            = 512;
            MemorySegment val_buffer = arena.allocate(buf_size);
            result = hdf5_h.H5Lget_val(H5fid, link_name, val_buffer, buf_size, hdf5_h.H5P_DEFAULT());
            assertTrue("H5Lget_val failed", isSuccess(result));

            // Unpack external link value
            MemorySegment file_ptr = allocateLong(arena); // Pointer to filename string
            MemorySegment obj_ptr  = allocateLong(arena); // Pointer to object path string

            result = hdf5_h.H5Lunpack_elink_val(val_buffer, buf_size, MemorySegment.NULL, file_ptr, obj_ptr);
            assertTrue("H5Lunpack_elink_val failed", isSuccess(result));
        }
    }

    // ============================================================================
    // Phase 4: Link Deletion
    // ============================================================================

    @Test
    public void testH5Ldelete()
    {
        try (Arena arena = Arena.ofConfined()) {
            // Create a link to delete
            MemorySegment src_name  = stringToSegment(arena, "Dataset1");
            MemorySegment link_name = stringToSegment(arena, "LinkToDelete");

            int result = hdf5_h.H5Lcreate_hard(H5fid, src_name, H5fid, link_name, hdf5_h.H5P_DEFAULT(),
                                               hdf5_h.H5P_DEFAULT());
            assertTrue("H5Lcreate_hard failed", isSuccess(result));

            // Verify link exists
            result = hdf5_h.H5Lexists(H5fid, link_name, hdf5_h.H5P_DEFAULT());
            assertTrue("Link should exist before deletion", result > 0);

            // Delete link
            result = hdf5_h.H5Ldelete(H5fid, link_name, hdf5_h.H5P_DEFAULT());
            assertTrue("H5Ldelete failed", isSuccess(result));

            // Verify link no longer exists
            result = hdf5_h.H5Lexists(H5fid, link_name, hdf5_h.H5P_DEFAULT());
            assertEquals("Link should not exist after deletion", 0, result);
        }
    }

    @Test
    public void testH5Ldelete_by_idx()
    {
        try (Arena arena = Arena.ofConfined()) {
            // Create multiple links in a group
            for (int i = 0; i < 3; i++) {
                MemorySegment src_name  = stringToSegment(arena, "Dataset1");
                MemorySegment link_name = stringToSegment(arena, "Link" + i);

                int result = hdf5_h.H5Lcreate_hard(H5fid, src_name, H5gid, link_name, hdf5_h.H5P_DEFAULT(),
                                                   hdf5_h.H5P_DEFAULT());
                assertTrue("H5Lcreate_hard failed", isSuccess(result));
            }

            // Delete link at index 1 by name order
            MemorySegment dotname = stringToSegment(arena, ".");
            int result = hdf5_h.H5Ldelete_by_idx(H5gid, dotname, hdf5_h.H5_INDEX_NAME(), hdf5_h.H5_ITER_INC(),
                                                 1, hdf5_h.H5P_DEFAULT());
            assertTrue("H5Ldelete_by_idx failed", isSuccess(result));

            // Verify link at index 1 is deleted (Link1)
            MemorySegment deleted_name = stringToSegment(arena, "Link1");
            result                     = hdf5_h.H5Lexists(H5gid, deleted_name, hdf5_h.H5P_DEFAULT());
            assertEquals("Link1 should not exist after deletion", 0, result);

            // Verify other links still exist
            MemorySegment link0_name = stringToSegment(arena, "Link0");
            result                   = hdf5_h.H5Lexists(H5gid, link0_name, hdf5_h.H5P_DEFAULT());
            assertTrue("Link0 should still exist", result > 0);
        }
    }

    // ============================================================================
    // Phase 5: Link Query Operations
    // ============================================================================

    @Test
    public void testH5Lexists()
    {
        try (Arena arena = Arena.ofConfined()) {
            // Check existing object
            MemorySegment existing_name = stringToSegment(arena, "Dataset1");
            int result                  = hdf5_h.H5Lexists(H5fid, existing_name, hdf5_h.H5P_DEFAULT());
            assertTrue("Dataset1 should exist", result > 0);

            // Check non-existing object
            MemorySegment non_existing_name = stringToSegment(arena, "DoesNotExist");
            result = hdf5_h.H5Lexists(H5fid, non_existing_name, hdf5_h.H5P_DEFAULT());
            assertEquals("DoesNotExist should not exist", 0, result);
        }
    }

    @Test
    public void testH5Lget_name_by_idx()
    {
        try (Arena arena = Arena.ofConfined()) {
            // Create multiple objects
            for (int i = 0; i < 3; i++) {
                MemorySegment src_name  = stringToSegment(arena, "Dataset1");
                MemorySegment link_name = stringToSegment(arena, "IndexLink" + i);

                int result = hdf5_h.H5Lcreate_hard(H5fid, src_name, H5fid, link_name, hdf5_h.H5P_DEFAULT(),
                                                   hdf5_h.H5P_DEFAULT());
                assertTrue("H5Lcreate_hard failed", isSuccess(result));
            }

            // Get name at index 1 by name order
            MemorySegment dotname = stringToSegment(arena, ".");
            long name_size =
                hdf5_h.H5Lget_name_by_idx(H5fid, dotname, hdf5_h.H5_INDEX_NAME(), hdf5_h.H5_ITER_INC(), 1,
                                          MemorySegment.NULL, 0, hdf5_h.H5P_DEFAULT());
            assertTrue("H5Lget_name_by_idx size query failed", name_size > 0);

            MemorySegment name_buffer = arena.allocate(name_size + 1);
            long actual_size =
                hdf5_h.H5Lget_name_by_idx(H5fid, dotname, hdf5_h.H5_INDEX_NAME(), hdf5_h.H5_ITER_INC(), 1,
                                          name_buffer, name_size + 1, hdf5_h.H5P_DEFAULT());
            assertTrue("H5Lget_name_by_idx failed", actual_size > 0);

            String retrieved_name = name_buffer.getString(0);
            assertTrue("Retrieved name should be a link name", retrieved_name.length() > 0);
        }
    }

    // ============================================================================
    // Phase 6: Comprehensive Workflow
    // ============================================================================

    @Test
    public void testH5L_complete_workflow()
    {
        try (Arena arena = Arena.ofConfined()) {
            // 1. Create hard link
            MemorySegment src_name  = stringToSegment(arena, "Dataset1");
            MemorySegment hard_link = stringToSegment(arena, "HardLinkWorkflow");

            int result = hdf5_h.H5Lcreate_hard(H5fid, src_name, H5fid, hard_link, hdf5_h.H5P_DEFAULT(),
                                               hdf5_h.H5P_DEFAULT());
            assertTrue("Create hard link failed", isSuccess(result));

            // 2. Create soft link
            MemorySegment soft_target = stringToSegment(arena, "/Dataset1");
            MemorySegment soft_link   = stringToSegment(arena, "SoftLinkWorkflow");

            result = hdf5_h.H5Lcreate_soft(soft_target, H5fid, soft_link, hdf5_h.H5P_DEFAULT(),
                                           hdf5_h.H5P_DEFAULT());
            assertTrue("Create soft link failed", isSuccess(result));

            // 3. Verify both exist
            result = hdf5_h.H5Lexists(H5fid, hard_link, hdf5_h.H5P_DEFAULT());
            assertTrue("Hard link should exist", result > 0);

            result = hdf5_h.H5Lexists(H5fid, soft_link, hdf5_h.H5P_DEFAULT());
            assertTrue("Soft link should exist", result > 0);

            // 4. Copy hard link
            MemorySegment copied_link = stringToSegment(arena, "CopiedLinkWorkflow");
            result = hdf5_h.H5Lcopy(H5fid, hard_link, H5fid, copied_link, hdf5_h.H5P_DEFAULT(),
                                    hdf5_h.H5P_DEFAULT());
            assertTrue("Copy link failed", isSuccess(result));

            // 5. Move soft link
            MemorySegment moved_link = stringToSegment(arena, "MovedSoftLinkWorkflow");
            result = hdf5_h.H5Lmove(H5fid, soft_link, H5fid, moved_link, hdf5_h.H5P_DEFAULT(),
                                    hdf5_h.H5P_DEFAULT());
            assertTrue("Move link failed", isSuccess(result));

            // 6. Verify soft link moved
            result = hdf5_h.H5Lexists(H5fid, soft_link, hdf5_h.H5P_DEFAULT());
            assertEquals("Old soft link should not exist", 0, result);

            result = hdf5_h.H5Lexists(H5fid, moved_link, hdf5_h.H5P_DEFAULT());
            assertTrue("Moved soft link should exist", result > 0);

            // 7. Delete copied link
            result = hdf5_h.H5Ldelete(H5fid, copied_link, hdf5_h.H5P_DEFAULT());
            assertTrue("Delete link failed", isSuccess(result));

            // 8. Verify deletion
            result = hdf5_h.H5Lexists(H5fid, copied_link, hdf5_h.H5P_DEFAULT());
            assertEquals("Copied link should not exist after deletion", 0, result);
        }
    }

    @Test
    public void testH5Lget_info()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create a group for testing
            MemorySegment groupName = stringToSegment(arena, "/test_group_info");
            long gid = hdf5_h.H5Gcreate2(H5fid, groupName, hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT(),
                                         hdf5_h.H5P_DEFAULT());
            assertTrue("H5Gcreate2 failed", isValidId(gid));

            // Create a link
            MemorySegment linkName = stringToSegment(arena, "/test_link_info");
            int result = hdf5_h.H5Lcreate_hard(H5fid, groupName, H5fid, linkName, hdf5_h.H5P_DEFAULT(),
                                               hdf5_h.H5P_DEFAULT());
            assertTrue("H5Lcreate_hard failed", isSuccess(result));

            // Get link info
            MemorySegment linfo = arena.allocate(56); // H5L_info_t size
            result              = hdf5_h.H5Lget_info2(H5fid, linkName, linfo, hdf5_h.H5P_DEFAULT());
            assertTrue("H5Lget_info2 failed", isSuccess(result));

            // Cleanup
            hdf5_h.H5Gclose(gid);
        }
    }

    @Test
    public void testH5Lget_info_by_idx()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create a group with some links
            MemorySegment groupName = stringToSegment(arena, "/test_group_idx");
            long gid = hdf5_h.H5Gcreate2(H5fid, groupName, hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT(),
                                         hdf5_h.H5P_DEFAULT());
            assertTrue("H5Gcreate2 failed", isValidId(gid));

            // Create subgroups to have links
            MemorySegment subgroup1 = stringToSegment(arena, "subgroup1");
            long gid1 = hdf5_h.H5Gcreate2(gid, subgroup1, hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT(),
                                          hdf5_h.H5P_DEFAULT());
            assertTrue("H5Gcreate2 subgroup1 failed", isValidId(gid1));

            // Get link info by index
            MemorySegment linfo = arena.allocate(56); // H5L_info_t size
            int result = hdf5_h.H5Lget_info_by_idx2(gid, stringToSegment(arena, "."), hdf5_h.H5_INDEX_NAME(),
                                                    hdf5_h.H5_ITER_INC(), 0, linfo, hdf5_h.H5P_DEFAULT());
            assertTrue("H5Lget_info_by_idx2 failed", isSuccess(result));

            // Cleanup
            hdf5_h.H5Gclose(gid1);
            hdf5_h.H5Gclose(gid);
        }
    }

    @Test
    public void testH5Lget_val()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create a soft link
            MemorySegment targetPath = stringToSegment(arena, "/target");
            MemorySegment linkName   = stringToSegment(arena, "/soft_link_val");
            int result = hdf5_h.H5Lcreate_soft(targetPath, H5fid, linkName, hdf5_h.H5P_DEFAULT(),
                                               hdf5_h.H5P_DEFAULT());
            assertTrue("H5Lcreate_soft failed", isSuccess(result));

            // Get link value size first
            MemorySegment size = allocateLongArray(arena, 1);
            result = hdf5_h.H5Lget_val(H5fid, linkName, MemorySegment.NULL, 0, hdf5_h.H5P_DEFAULT());
            assertTrue("H5Lget_val (size query) should succeed", isSuccess(result));

            // Note: H5Lget_val returns size via return value in some versions
            // For FFM testing, we verify the call succeeds
        }
    }

    @Test
    public void testH5Lget_val_by_idx()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create a group
            MemorySegment groupName = stringToSegment(arena, "/test_group_val_idx");
            long gid = hdf5_h.H5Gcreate2(H5fid, groupName, hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT(),
                                         hdf5_h.H5P_DEFAULT());
            assertTrue("H5Gcreate2 failed", isValidId(gid));

            // Create a soft link inside the group
            MemorySegment targetPath = stringToSegment(arena, "/target");
            MemorySegment linkName   = stringToSegment(arena, "soft_link");
            int result =
                hdf5_h.H5Lcreate_soft(targetPath, gid, linkName, hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());
            assertTrue("H5Lcreate_soft failed", isSuccess(result));

            // Get link value by index
            result = hdf5_h.H5Lget_val_by_idx(gid, stringToSegment(arena, "."), hdf5_h.H5_INDEX_NAME(),
                                              hdf5_h.H5_ITER_INC(), 0, MemorySegment.NULL, 0,
                                              hdf5_h.H5P_DEFAULT());
            assertTrue("H5Lget_val_by_idx should succeed", isSuccess(result));

            // Cleanup
            hdf5_h.H5Gclose(gid);
        }
    }

    @Test
    public void testH5Lget_name_by_idx_multiple()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create a group with multiple links
            MemorySegment groupName = stringToSegment(arena, "/test_multiple_links");
            long gid = hdf5_h.H5Gcreate2(H5fid, groupName, hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT(),
                                         hdf5_h.H5P_DEFAULT());
            assertTrue("H5Gcreate2 failed", isValidId(gid));

            // Create multiple subgroups
            for (int i = 0; i < 3; i++) {
                MemorySegment subName = stringToSegment(arena, "sub" + i);
                long subGid = hdf5_h.H5Gcreate2(gid, subName, hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT(),
                                                hdf5_h.H5P_DEFAULT());
                assertTrue("H5Gcreate2 sub" + i + " failed", isValidId(subGid));
                hdf5_h.H5Gclose(subGid);
            }

            // Get name of first link by index
            long nameSize = hdf5_h.H5Lget_name_by_idx(gid, stringToSegment(arena, "."),
                                                      hdf5_h.H5_INDEX_NAME(), hdf5_h.H5_ITER_INC(), 0,
                                                      MemorySegment.NULL, 0, hdf5_h.H5P_DEFAULT());
            assertTrue("Name size should be > 0", nameSize > 0);

            // Get the actual name
            MemorySegment nameBuf = arena.allocate(nameSize + 1);
            long actualSize       = hdf5_h.H5Lget_name_by_idx(gid, stringToSegment(arena, "."),
                                                              hdf5_h.H5_INDEX_NAME(), hdf5_h.H5_ITER_INC(), 0,
                                                              nameBuf, nameSize + 1, hdf5_h.H5P_DEFAULT());
            assertTrue("Actual size should match", actualSize == nameSize);

            String linkName = segmentToString(nameBuf);
            assertFalse("Link name should not be empty", linkName.isEmpty());

            // Cleanup
            hdf5_h.H5Gclose(gid);
        }
    }
}
