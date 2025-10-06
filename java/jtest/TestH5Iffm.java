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
import org.hdfgroup.javahdf5.hdf5_h_2;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

/**
 * FFM-only tests for HDF5 Identifier (H5I) operations.
 */
public class TestH5Iffm {
    @Rule
    public TestName testname = new TestName();

    private static final String H5_FILE = "test_H5Iffm.h5";

    long H5fid = hdf5_h.H5I_INVALID_HID();
    long H5gid = hdf5_h.H5I_INVALID_HID();

    @Before
    public void createH5file()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            MemorySegment filename = stringToSegment(arena, H5_FILE);
            H5fid = hdf5_h_1.H5Fcreate(filename, hdf5_h.H5F_ACC_TRUNC(), hdf5_h.H5P_DEFAULT(),
                                       hdf5_h.H5P_DEFAULT());
            assertTrue("H5Fcreate failed", isValidId(H5fid));

            MemorySegment groupname = stringToSegment(arena, "Group1");
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

    @Test
    public void testH5Iget_type()
    {
        int file_type = hdf5_h_2.H5Iget_type(H5fid);
        assertEquals("File type should be H5I_FILE", hdf5_h.H5I_FILE(), file_type);

        int group_type = hdf5_h_2.H5Iget_type(H5gid);
        assertEquals("Group type should be H5I_GROUP", hdf5_h.H5I_GROUP(), group_type);
    }

    @Test
    public void testH5Iis_valid()
    {
        int result = hdf5_h_2.H5Iis_valid(H5fid);
        assertTrue("File ID should be valid", result > 0);

        result = hdf5_h_2.H5Iis_valid(H5gid);
        assertTrue("Group ID should be valid", result > 0);

        result = hdf5_h_2.H5Iis_valid(hdf5_h.H5I_INVALID_HID());
        assertEquals("Invalid ID should not be valid", 0, result);
    }

    @Test
    public void testH5Iget_name()
    {
        try (Arena arena = Arena.ofConfined()) {
            // Get group name size
            long name_size = hdf5_h_2.H5Iget_name(H5gid, MemorySegment.NULL, 0);
            assertTrue("H5Iget_name size query failed", name_size > 0);

            // Get group name
            MemorySegment nameBuffer = arena.allocate(name_size + 1);
            long actual_size         = hdf5_h_2.H5Iget_name(H5gid, nameBuffer, name_size + 1);
            assertTrue("H5Iget_name failed", actual_size > 0);

            String name = nameBuffer.getString(0);
            assertEquals("Group name should be /Group1", "/Group1", name);
        }
    }

    @Test
    public void testH5Iget_file_id()
    {
        long file_id = hdf5_h_2.H5Iget_file_id(H5gid);
        assertTrue("H5Iget_file_id failed", isValidId(file_id));

        int type = hdf5_h_2.H5Iget_type(file_id);
        assertEquals("Should be file type", hdf5_h.H5I_FILE(), type);

        hdf5_h_1.H5Fclose(file_id);
    }

    @Test
    public void testH5Iinc_dec_ref()
    {
        // Get initial ref count
        int ref_count = hdf5_h_2.H5Iget_ref(H5gid);
        assertTrue("Initial ref count should be positive", ref_count > 0);

        // Increment ref count
        int new_count = hdf5_h_2.H5Iinc_ref(H5gid);
        assertEquals("Ref count should increase by 1", ref_count + 1, new_count);

        // Decrement ref count
        new_count = hdf5_h_2.H5Idec_ref(H5gid);
        assertEquals("Ref count should decrease by 1", ref_count, new_count);
    }

    @Test
    public void testH5I_complete_workflow()
    {
        try (Arena arena = Arena.ofConfined()) {
            // 1. Verify ID is valid
            int result = hdf5_h_2.H5Iis_valid(H5gid);
            assertTrue("ID should be valid", result > 0);

            // 2. Get type
            int type = hdf5_h_2.H5Iget_type(H5gid);
            assertEquals("Type should be GROUP", hdf5_h.H5I_GROUP(), type);

            // 3. Get name
            long name_size = hdf5_h_2.H5Iget_name(H5gid, MemorySegment.NULL, 0);
            assertTrue("Name size should be positive", name_size > 0);

            // 4. Get file ID
            long file_id = hdf5_h_2.H5Iget_file_id(H5gid);
            assertTrue("File ID should be valid", isValidId(file_id));

            // 5. Get ref count
            int ref_count = hdf5_h_2.H5Iget_ref(H5gid);
            assertTrue("Ref count should be positive", ref_count > 0);

            hdf5_h_1.H5Fclose(file_id);
        }
    }
}
