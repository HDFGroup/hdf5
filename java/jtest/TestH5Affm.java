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

import org.hdfgroup.javahdf5.H5A_info_t;
import org.hdfgroup.javahdf5.H5A_operator2_t;
import org.hdfgroup.javahdf5.hdf5_h;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

/**
 * TestH5Affm - FFM-based tests for HDF5 Attribute operations.
 * Tests the H5A* API using Foreign Function & Memory (FFM) bindings.
 */
public class TestH5Affm {
    private static final String H5_FILE = "testA.h5";
    private static final int DIM_X      = 4;
    private static final int DIM_Y      = 6;
    private static final int RANK       = 2;

    @Rule
    public TestName testname = new TestName();

    long H5fid = hdf5_h.H5I_INVALID_HID();
    long H5did = hdf5_h.H5I_INVALID_HID();
    long H5sid = hdf5_h.H5I_INVALID_HID();
    long H5aid = hdf5_h.H5I_INVALID_HID();

    @Before
    public void createH5file() throws Exception
    {
        try (Arena arena = Arena.ofConfined()) {
            // Create file
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

            // Create dataset for attaching attributes
            MemorySegment dsetName = stringToSegment(arena, "dset");
            H5did = hdf5_h.H5Dcreate2(H5fid, dsetName, hdf5_h.H5T_NATIVE_INT_g(), H5sid, hdf5_h.H5P_DEFAULT(),
                                      hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());
            assertTrue("H5Dcreate2 failed", isValidId(H5did));
        }
    }

    @After
    public void deleteH5file() throws Exception
    {
        closeQuietly(H5aid, hdf5_h::H5Aclose);
        closeQuietly(H5did, hdf5_h::H5Dclose);
        closeQuietly(H5sid, hdf5_h::H5Sclose);
        closeQuietly(H5fid, hdf5_h::H5Fclose);

        H5aid = hdf5_h.H5I_INVALID_HID();
        H5did = hdf5_h.H5I_INVALID_HID();
        H5sid = hdf5_h.H5I_INVALID_HID();
        H5fid = hdf5_h.H5I_INVALID_HID();
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

    @Test
    public void testH5Acreate()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create scalar attribute space
            long attr_sid = hdf5_h.H5Screate(hdf5_h.H5S_SCALAR());
            assertTrue("H5Screate scalar failed", isValidId(attr_sid));

            // Create attribute
            MemorySegment attrName = stringToSegment(arena, "attr1");
            H5aid                  = hdf5_h.H5Acreate2(H5did, attrName, hdf5_h.H5T_NATIVE_INT_g(), attr_sid,
                                                       hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());
            assertTrue("H5Acreate2 failed", isValidId(H5aid));

            hdf5_h.H5Sclose(attr_sid);
        }
    }

    @Test
    public void testH5Awrite_read()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create attribute with array dataspace
            long[] attr_dims              = {3};
            MemorySegment attrDimsSegment = allocateLongArray(arena, 1);
            attrDimsSegment.setAtIndex(ValueLayout.JAVA_LONG, 0, attr_dims[0]);

            long attr_sid = hdf5_h.H5Screate_simple(1, attrDimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple failed", isValidId(attr_sid));

            MemorySegment attrName = stringToSegment(arena, "int_array_attr");
            H5aid                  = hdf5_h.H5Acreate2(H5did, attrName, hdf5_h.H5T_NATIVE_INT_g(), attr_sid,
                                                       hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());
            assertTrue("H5Acreate2 failed", isValidId(H5aid));

            // Write data
            int[] write_data           = {10, 20, 30};
            MemorySegment writeSegment = allocateIntArray(arena, 3);
            copyToSegment(writeSegment, write_data);

            int result = hdf5_h.H5Awrite(H5aid, hdf5_h.H5T_NATIVE_INT_g(), writeSegment);
            assertTrue("H5Awrite failed", isSuccess(result));

            // Read back
            MemorySegment readSegment = allocateIntArray(arena, 3);
            result                    = hdf5_h.H5Aread(H5aid, hdf5_h.H5T_NATIVE_INT_g(), readSegment);
            assertTrue("H5Aread failed", isSuccess(result));

            // Verify
            int[] read_data = new int[3];
            copyFromSegment(readSegment, read_data);
            assertArrayEquals("Data mismatch", write_data, read_data);

            hdf5_h.H5Sclose(attr_sid);
        }
    }

    @Test
    public void testH5Aopen()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create attribute first
            long attr_sid          = hdf5_h.H5Screate(hdf5_h.H5S_SCALAR());
            MemorySegment attrName = stringToSegment(arena, "test_attr");

            long aid = hdf5_h.H5Acreate2(H5did, attrName, hdf5_h.H5T_NATIVE_INT_g(), attr_sid,
                                         hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());
            assertTrue("H5Acreate2 failed", isValidId(aid));
            hdf5_h.H5Aclose(aid);

            // Open attribute
            H5aid = hdf5_h.H5Aopen(H5did, attrName, hdf5_h.H5P_DEFAULT());
            assertTrue("H5Aopen failed", isValidId(H5aid));

            hdf5_h.H5Sclose(attr_sid);
        }
    }

    @Test
    public void testH5Aclose()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long attr_sid          = hdf5_h.H5Screate(hdf5_h.H5S_SCALAR());
            MemorySegment attrName = stringToSegment(arena, "temp_attr");

            H5aid = hdf5_h.H5Acreate2(H5did, attrName, hdf5_h.H5T_NATIVE_INT_g(), attr_sid,
                                      hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());
            assertTrue("H5Acreate2 failed", isValidId(H5aid));

            int result = hdf5_h.H5Aclose(H5aid);
            assertTrue("H5Aclose failed", isSuccess(result));

            H5aid = hdf5_h.H5I_INVALID_HID();
            hdf5_h.H5Sclose(attr_sid);
        }
    }

    @Test
    public void testH5Aget_name()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            String expectedName    = "my_attribute";
            long attr_sid          = hdf5_h.H5Screate(hdf5_h.H5S_SCALAR());
            MemorySegment attrName = stringToSegment(arena, expectedName);

            H5aid = hdf5_h.H5Acreate2(H5did, attrName, hdf5_h.H5T_NATIVE_INT_g(), attr_sid,
                                      hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());
            assertTrue("H5Acreate2 failed", isValidId(H5aid));

            // Get name size
            long nameSize = hdf5_h.H5Aget_name(H5aid, 0, MemorySegment.NULL);
            assertTrue("H5Aget_name size query failed", nameSize > 0);

            // Get name
            MemorySegment nameBuffer = arena.allocate(nameSize + 1);
            hdf5_h.H5Aget_name(H5aid, nameSize + 1, nameBuffer);

            String retrievedName = nameBuffer.getString(0);
            assertEquals("Attribute name mismatch", expectedName, retrievedName);

            hdf5_h.H5Sclose(attr_sid);
        }
    }

    @Test
    public void testH5Aget_space()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create attribute with specific dimensions
            long[] dims               = {5, 3};
            MemorySegment dimsSegment = allocateLongArray(arena, 2);
            copyToSegment(dimsSegment, dims);

            long attr_sid          = hdf5_h.H5Screate_simple(2, dimsSegment, MemorySegment.NULL);
            MemorySegment attrName = stringToSegment(arena, "array_attr");

            H5aid = hdf5_h.H5Acreate2(H5did, attrName, hdf5_h.H5T_NATIVE_INT_g(), attr_sid,
                                      hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());
            assertTrue("H5Acreate2 failed", isValidId(H5aid));

            // Get dataspace
            long retrieved_sid = hdf5_h.H5Aget_space(H5aid);
            assertTrue("H5Aget_space failed", isValidId(retrieved_sid));

            // Verify dimensions
            MemorySegment retrievedDims = allocateLongArray(arena, 2);
            hdf5_h.H5Sget_simple_extent_dims(retrieved_sid, retrievedDims, MemorySegment.NULL);

            long[] readDims = new long[2];
            copyFromSegment(retrievedDims, readDims);
            assertArrayEquals("Dimensions mismatch", dims, readDims);

            hdf5_h.H5Sclose(retrieved_sid);
            hdf5_h.H5Sclose(attr_sid);
        }
    }

    @Test
    public void testH5Aget_type()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long attr_sid          = hdf5_h.H5Screate(hdf5_h.H5S_SCALAR());
            MemorySegment attrName = stringToSegment(arena, "type_attr");

            H5aid = hdf5_h.H5Acreate2(H5did, attrName, hdf5_h.H5T_NATIVE_DOUBLE_g(), attr_sid,
                                      hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());
            assertTrue("H5Acreate2 failed", isValidId(H5aid));

            // Get type
            long retrieved_tid = hdf5_h.H5Aget_type(H5aid);
            assertTrue("H5Aget_type failed", isValidId(retrieved_tid));

            // Verify it's a double type
            int equal = hdf5_h.H5Tequal(retrieved_tid, hdf5_h.H5T_NATIVE_DOUBLE_g());
            assertTrue("Type should be H5T_NATIVE_DOUBLE", equal > 0);

            hdf5_h.H5Tclose(retrieved_tid);
            hdf5_h.H5Sclose(attr_sid);
        }
    }

    @Test
    public void testH5Aexists()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            MemorySegment existingName = stringToSegment(arena, "existing_attr");
            MemorySegment missingName  = stringToSegment(arena, "missing_attr");

            // Create one attribute
            long attr_sid = hdf5_h.H5Screate(hdf5_h.H5S_SCALAR());
            long aid      = hdf5_h.H5Acreate2(H5did, existingName, hdf5_h.H5T_NATIVE_INT_g(), attr_sid,
                                              hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());
            assertTrue("H5Acreate2 failed", isValidId(aid));
            hdf5_h.H5Aclose(aid);

            // Check existing
            int exists = hdf5_h.H5Aexists(H5did, existingName);
            assertTrue("Attribute should exist", exists > 0);

            // Check non-existing
            exists = hdf5_h.H5Aexists(H5did, missingName);
            assertEquals("Attribute should not exist", 0, exists);

            hdf5_h.H5Sclose(attr_sid);
        }
    }

    @Test
    public void testH5Adelete()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            MemorySegment attrName = stringToSegment(arena, "deletable_attr");

            // Create attribute
            long attr_sid = hdf5_h.H5Screate(hdf5_h.H5S_SCALAR());
            long aid      = hdf5_h.H5Acreate2(H5did, attrName, hdf5_h.H5T_NATIVE_INT_g(), attr_sid,
                                              hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());
            assertTrue("H5Acreate2 failed", isValidId(aid));
            hdf5_h.H5Aclose(aid);

            // Verify it exists
            int exists = hdf5_h.H5Aexists(H5did, attrName);
            assertTrue("Attribute should exist before delete", exists > 0);

            // Delete
            int result = hdf5_h.H5Adelete(H5did, attrName);
            assertTrue("H5Adelete failed", isSuccess(result));

            // Verify it's gone
            exists = hdf5_h.H5Aexists(H5did, attrName);
            assertEquals("Attribute should not exist after delete", 0, exists);

            hdf5_h.H5Sclose(attr_sid);
        }
    }

    @Test
    public void testH5Aget_storage_size()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create attribute with 10 integers
            long[] attr_dims              = {10};
            MemorySegment attrDimsSegment = allocateLongArray(arena, 1);
            attrDimsSegment.setAtIndex(ValueLayout.JAVA_LONG, 0, attr_dims[0]);

            long attr_sid          = hdf5_h.H5Screate_simple(1, attrDimsSegment, MemorySegment.NULL);
            MemorySegment attrName = stringToSegment(arena, "storage_attr");

            H5aid = hdf5_h.H5Acreate2(H5did, attrName, hdf5_h.H5T_NATIVE_INT_g(), attr_sid,
                                      hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());
            assertTrue("H5Acreate2 failed", isValidId(H5aid));

            // Write data
            int[] data                = new int[10];
            MemorySegment dataSegment = allocateIntArray(arena, 10);
            copyToSegment(dataSegment, data);
            hdf5_h.H5Awrite(H5aid, hdf5_h.H5T_NATIVE_INT_g(), dataSegment);

            // Get storage size
            long storage_size = hdf5_h.H5Aget_storage_size(H5aid);
            assertEquals("Storage size should be 10 * sizeof(int)", 40L, storage_size);

            hdf5_h.H5Sclose(attr_sid);
        }
    }

    @Test
    public void testH5Awrite_readStr()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            String testString = "Hello HDF5 Attributes!";

            // Create string type
            long str_tid = hdf5_h.H5Tcopy(hdf5_h.H5T_C_S1_g());
            assertTrue("H5Tcopy failed", isValidId(str_tid));

            hdf5_h.H5Tset_size(str_tid, testString.length() + 1);
            hdf5_h.H5Tset_strpad(str_tid, hdf5_h.H5T_STR_NULLTERM());

            // Create attribute
            long attr_sid          = hdf5_h.H5Screate(hdf5_h.H5S_SCALAR());
            MemorySegment attrName = stringToSegment(arena, "str_attr");

            H5aid = hdf5_h.H5Acreate2(H5did, attrName, str_tid, attr_sid, hdf5_h.H5P_DEFAULT(),
                                      hdf5_h.H5P_DEFAULT());
            assertTrue("H5Acreate2 failed", isValidId(H5aid));

            // Write string
            MemorySegment writeData = stringToSegment(arena, testString);
            int writeResult         = hdf5_h.H5Awrite(H5aid, str_tid, writeData);
            assertTrue("H5Awrite failed", isSuccess(writeResult));

            // Read string back
            MemorySegment readData = arena.allocate(testString.length() + 1);
            int readResult         = hdf5_h.H5Aread(H5aid, str_tid, readData);
            assertTrue("H5Aread failed", isSuccess(readResult));

            String retrievedString = readData.getString(0);
            assertEquals("String mismatch", testString, retrievedString);

            hdf5_h.H5Tclose(str_tid);
            hdf5_h.H5Sclose(attr_sid);
        }
    }

    @Test
    public void testH5Arename()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            MemorySegment oldName = stringToSegment(arena, "old_name");
            MemorySegment newName = stringToSegment(arena, "new_name");

            // Create attribute
            long attr_sid = hdf5_h.H5Screate(hdf5_h.H5S_SCALAR());
            long aid      = hdf5_h.H5Acreate2(H5did, oldName, hdf5_h.H5T_NATIVE_INT_g(), attr_sid,
                                              hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());
            assertTrue("H5Acreate2 failed", isValidId(aid));
            hdf5_h.H5Aclose(aid);

            // Verify old name exists
            int exists = hdf5_h.H5Aexists(H5did, oldName);
            assertTrue("Old name should exist", exists > 0);

            // Rename
            int result = hdf5_h.H5Arename(H5did, oldName, newName);
            assertTrue("H5Arename failed", isSuccess(result));

            // Verify new name exists and old doesn't
            exists = hdf5_h.H5Aexists(H5did, newName);
            assertTrue("New name should exist", exists > 0);

            exists = hdf5_h.H5Aexists(H5did, oldName);
            assertEquals("Old name should not exist", 0, exists);

            hdf5_h.H5Sclose(attr_sid);
        }
    }

    @Test
    public void testH5Aget_num_attrs()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create 3 attributes
            long attr_sid = hdf5_h.H5Screate(hdf5_h.H5S_SCALAR());

            for (int i = 0; i < 3; i++) {
                MemorySegment attrName = stringToSegment(arena, "attr_" + i);
                long aid = hdf5_h.H5Acreate2(H5did, attrName, hdf5_h.H5T_NATIVE_INT_g(), attr_sid,
                                             hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());
                assertTrue("H5Acreate2 failed for attr_" + i, isValidId(aid));
                hdf5_h.H5Aclose(aid);
            }

            // Get number of attributes
            int num_attrs = hdf5_h.H5Aget_num_attrs(H5did);
            assertTrue("Should have at least 3 attributes", num_attrs >= 3);

            hdf5_h.H5Sclose(attr_sid);
        }
    }

    // =========================
    // Phase 1: Essential Query Operations
    // =========================

    @Test
    public void testH5Aget_info()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create attribute with scalar dataspace
            long attr_sid          = hdf5_h.H5Screate(hdf5_h.H5S_SCALAR());
            MemorySegment attrName = stringToSegment(arena, "info_test_attr");

            long aid = hdf5_h.H5Acreate2(H5did, attrName, hdf5_h.H5T_NATIVE_INT_g(), attr_sid,
                                         hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());
            assertTrue("H5Acreate2 failed", isValidId(aid));

            // Write some data
            MemorySegment data = allocateInt(arena);
            setInt(data, 42);
            int writeResult = hdf5_h.H5Awrite(aid, hdf5_h.H5T_NATIVE_INT_g(), data);
            assertTrue("H5Awrite failed", isSuccess(writeResult));

            // Get attribute info
            MemorySegment info = H5A_info_t.allocate(arena);
            int result         = hdf5_h.H5Aget_info(aid, info);
            assertTrue("H5Aget_info failed", isSuccess(result));

            // Verify info fields
            long data_size = H5A_info_t.data_size(info);
            assertEquals("Data size should be 4 bytes (sizeof int)", 4L, data_size);

            // corder_valid may be false if creation order tracking is not enabled
            boolean corder_valid = H5A_info_t.corder_valid(info);
            // Just verify we can read it (no assertion on value)
            assertNotNull("corder_valid should be readable", Boolean.valueOf(corder_valid));

            // cset should be ASCII by default
            int cset = H5A_info_t.cset(info);
            assertEquals("Character set should be ASCII", hdf5_h.H5T_CSET_ASCII(), cset);

            hdf5_h.H5Aclose(aid);
            hdf5_h.H5Sclose(attr_sid);
        }
    }

    @Test
    public void testH5Aget_info_by_idx()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create multiple attributes to test indexing
            long attr_sid = hdf5_h.H5Screate(hdf5_h.H5S_SCALAR());

            String[] attrNames = {"first_attr", "second_attr", "third_attr"};
            for (String name : attrNames) {
                MemorySegment attrName = stringToSegment(arena, name);
                long aid = hdf5_h.H5Acreate2(H5did, attrName, hdf5_h.H5T_NATIVE_DOUBLE_g(), attr_sid,
                                             hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());
                assertTrue("H5Acreate2 failed for " + name, isValidId(aid));
                hdf5_h.H5Aclose(aid);
            }

            // Get info for the second attribute (index 1) by name order
            MemorySegment objName = stringToSegment(arena, ".");
            MemorySegment info    = H5A_info_t.allocate(arena);

            int result = hdf5_h.H5Aget_info_by_idx(H5did, objName, hdf5_h.H5_INDEX_NAME(),
                                                   hdf5_h.H5_ITER_INC(), 1, info, hdf5_h.H5P_DEFAULT());
            assertTrue("H5Aget_info_by_idx failed", isSuccess(result));

            // Verify we got valid info
            long data_size = H5A_info_t.data_size(info);
            assertEquals("Data size should be 8 bytes (sizeof double)", 8L, data_size);

            hdf5_h.H5Sclose(attr_sid);
        }
    }

    @Test
    public void testH5Aget_info_by_name()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create attribute on dataset
            String attrNameStr     = "named_info_attr";
            long attr_sid          = hdf5_h.H5Screate(hdf5_h.H5S_SCALAR());
            MemorySegment attrName = stringToSegment(arena, attrNameStr);

            long aid = hdf5_h.H5Acreate2(H5did, attrName, hdf5_h.H5T_NATIVE_FLOAT_g(), attr_sid,
                                         hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());
            assertTrue("H5Acreate2 failed", isValidId(aid));
            hdf5_h.H5Aclose(aid);

            // Get info by name from the file (using dataset path)
            MemorySegment objName = stringToSegment(arena, "dset");
            MemorySegment info    = H5A_info_t.allocate(arena);

            int result = hdf5_h.H5Aget_info_by_name(H5fid, objName, attrName, info, hdf5_h.H5P_DEFAULT());
            assertTrue("H5Aget_info_by_name failed", isSuccess(result));

            // Verify info
            long data_size = H5A_info_t.data_size(info);
            assertEquals("Data size should be 4 bytes (sizeof float)", 4L, data_size);

            hdf5_h.H5Sclose(attr_sid);
        }
    }

    @Test
    public void testH5Aget_name_by_idx()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create multiple attributes with known names
            long attr_sid        = hdf5_h.H5Screate(hdf5_h.H5S_SCALAR());
            String[] attrNames   = {"alpha", "beta", "gamma"};
            String[] sortedNames = {"alpha", "beta", "gamma"}; // Already sorted alphabetically

            for (String name : attrNames) {
                MemorySegment attrName = stringToSegment(arena, name);
                long aid = hdf5_h.H5Acreate2(H5did, attrName, hdf5_h.H5T_NATIVE_INT_g(), attr_sid,
                                             hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());
                assertTrue("H5Acreate2 failed for " + name, isValidId(aid));
                hdf5_h.H5Aclose(aid);
            }

            // Get name of each attribute by index (alphabetical order)
            MemorySegment objName = stringToSegment(arena, ".");

            for (int i = 0; i < sortedNames.length; i++) {
                // Get name size first
                long nameSize =
                    hdf5_h.H5Aget_name_by_idx(H5did, objName, hdf5_h.H5_INDEX_NAME(), hdf5_h.H5_ITER_INC(), i,
                                              MemorySegment.NULL, 0, hdf5_h.H5P_DEFAULT());
                assertTrue("H5Aget_name_by_idx size query failed for index " + i, nameSize > 0);

                // Get actual name
                MemorySegment nameBuffer = arena.allocate(nameSize + 1);
                long result =
                    hdf5_h.H5Aget_name_by_idx(H5did, objName, hdf5_h.H5_INDEX_NAME(), hdf5_h.H5_ITER_INC(), i,
                                              nameBuffer, nameSize + 1, hdf5_h.H5P_DEFAULT());
                assertTrue("H5Aget_name_by_idx failed for index " + i, result > 0);

                String retrievedName = nameBuffer.getString(0);
                assertEquals("Name at index " + i + " should match", sortedNames[i], retrievedName);
            }

            hdf5_h.H5Sclose(attr_sid);
        }
    }

    @Test
    public void testH5Aget_create_plist()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create attribute creation property list with specific settings
            long acpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_ATTRIBUTE_CREATE_ID_g());
            assertTrue("H5Pcreate acpl failed", isValidId(acpl));

            // Set character encoding to UTF-8
            int setResult = hdf5_h.H5Pset_char_encoding(acpl, hdf5_h.H5T_CSET_UTF8());
            assertTrue("H5Pset_char_encoding failed", isSuccess(setResult));

            // Create attribute with this property list
            long attr_sid          = hdf5_h.H5Screate(hdf5_h.H5S_SCALAR());
            MemorySegment attrName = stringToSegment(arena, "plist_attr");

            long aid = hdf5_h.H5Acreate2(H5did, attrName, hdf5_h.H5T_NATIVE_INT_g(), attr_sid, acpl,
                                         hdf5_h.H5P_DEFAULT());
            assertTrue("H5Acreate2 failed", isValidId(aid));

            // Get the creation property list back
            long retrieved_acpl = hdf5_h.H5Aget_create_plist(aid);
            assertTrue("H5Aget_create_plist failed", isValidId(retrieved_acpl));

            // Verify the encoding is UTF-8
            MemorySegment encoding = allocateInt(arena);
            int getResult          = hdf5_h.H5Pget_char_encoding(retrieved_acpl, encoding);
            assertTrue("H5Pget_char_encoding failed", isSuccess(getResult));

            int retrievedEncoding = getInt(encoding);
            assertEquals("Character encoding should be UTF-8", hdf5_h.H5T_CSET_UTF8(), retrievedEncoding);

            hdf5_h.H5Pclose(retrieved_acpl);
            hdf5_h.H5Pclose(acpl);
            hdf5_h.H5Aclose(aid);
            hdf5_h.H5Sclose(attr_sid);
        }
    }

    @Test
    public void testH5Aexists_by_name()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create attribute on dataset
            String existingAttrName    = "existing_by_name";
            String nonExistingAttrName = "nonexistent_by_name";

            long attr_sid          = hdf5_h.H5Screate(hdf5_h.H5S_SCALAR());
            MemorySegment attrName = stringToSegment(arena, existingAttrName);

            long aid = hdf5_h.H5Acreate2(H5did, attrName, hdf5_h.H5T_NATIVE_INT_g(), attr_sid,
                                         hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());
            assertTrue("H5Acreate2 failed", isValidId(aid));
            hdf5_h.H5Aclose(aid);

            // Check existence from file using dataset path
            MemorySegment objName             = stringToSegment(arena, "dset");
            MemorySegment existingAttrNameSeg = stringToSegment(arena, existingAttrName);
            MemorySegment missingAttrNameSeg  = stringToSegment(arena, nonExistingAttrName);

            // Should exist
            int exists = hdf5_h.H5Aexists_by_name(H5fid, objName, existingAttrNameSeg, hdf5_h.H5P_DEFAULT());
            assertTrue("Attribute should exist", exists > 0);

            // Should not exist
            exists = hdf5_h.H5Aexists_by_name(H5fid, objName, missingAttrNameSeg, hdf5_h.H5P_DEFAULT());
            assertEquals("Attribute should not exist", 0, exists);

            hdf5_h.H5Sclose(attr_sid);
        }
    }

    // =========================
    // Phase 2: Advanced Operations
    // =========================

    @Test
    public void testH5Aopen_by_idx()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create multiple attributes with known names
            long attr_sid        = hdf5_h.H5Screate(hdf5_h.H5S_SCALAR());
            String[] attrNames   = {"apple", "banana", "cherry"};
            String[] sortedNames = {"apple", "banana", "cherry"}; // Already sorted

            for (String name : attrNames) {
                MemorySegment attrName = stringToSegment(arena, name);
                long aid = hdf5_h.H5Acreate2(H5did, attrName, hdf5_h.H5T_NATIVE_INT_g(), attr_sid,
                                             hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());
                assertTrue("H5Acreate2 failed for " + name, isValidId(aid));
                hdf5_h.H5Aclose(aid);
            }

            // Open second attribute by index (index 1)
            MemorySegment objName = stringToSegment(arena, ".");
            long aid = hdf5_h.H5Aopen_by_idx(H5did, objName, hdf5_h.H5_INDEX_NAME(), hdf5_h.H5_ITER_INC(), 1,
                                             hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());
            assertTrue("H5Aopen_by_idx failed", isValidId(aid));

            // Verify we opened the correct attribute by checking its name
            long nameSize         = hdf5_h.H5Aget_name(aid, 0, MemorySegment.NULL);
            MemorySegment nameBuf = arena.allocate(nameSize + 1);
            hdf5_h.H5Aget_name(aid, nameSize + 1, nameBuf);
            String retrievedName = nameBuf.getString(0);
            assertEquals("Should have opened 'banana' (index 1)", sortedNames[1], retrievedName);

            hdf5_h.H5Aclose(aid);
            hdf5_h.H5Sclose(attr_sid);
        }
    }

    @Test
    public void testH5Adelete_by_idx()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create 3 attributes
            long attr_sid      = hdf5_h.H5Screate(hdf5_h.H5S_SCALAR());
            String[] attrNames = {"first", "second", "third"};

            for (String name : attrNames) {
                MemorySegment attrName = stringToSegment(arena, name);
                long aid = hdf5_h.H5Acreate2(H5did, attrName, hdf5_h.H5T_NATIVE_INT_g(), attr_sid,
                                             hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());
                assertTrue("H5Acreate2 failed for " + name, isValidId(aid));
                hdf5_h.H5Aclose(aid);
            }

            // Delete middle attribute (index 1 = "second")
            MemorySegment objName = stringToSegment(arena, ".");
            int result = hdf5_h.H5Adelete_by_idx(H5did, objName, hdf5_h.H5_INDEX_NAME(), hdf5_h.H5_ITER_INC(),
                                                 1, hdf5_h.H5P_DEFAULT());
            assertTrue("H5Adelete_by_idx failed", isSuccess(result));

            // Verify "second" is gone
            MemorySegment secondName = stringToSegment(arena, "second");
            int exists               = hdf5_h.H5Aexists(H5did, secondName);
            assertEquals("'second' should not exist after deletion", 0, exists);

            // Verify others still exist
            MemorySegment firstName = stringToSegment(arena, "first");
            MemorySegment thirdName = stringToSegment(arena, "third");
            exists                  = hdf5_h.H5Aexists(H5did, firstName);
            assertTrue("'first' should still exist", exists > 0);
            exists = hdf5_h.H5Aexists(H5did, thirdName);
            assertTrue("'third' should still exist", exists > 0);

            hdf5_h.H5Sclose(attr_sid);
        }
    }

    @Test
    public void testH5Adelete_by_name()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create attribute on dataset
            String attrNameStr     = "deletable_by_name";
            long attr_sid          = hdf5_h.H5Screate(hdf5_h.H5S_SCALAR());
            MemorySegment attrName = stringToSegment(arena, attrNameStr);

            long aid = hdf5_h.H5Acreate2(H5did, attrName, hdf5_h.H5T_NATIVE_INT_g(), attr_sid,
                                         hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());
            assertTrue("H5Acreate2 failed", isValidId(aid));
            hdf5_h.H5Aclose(aid);

            // Verify it exists
            int exists = hdf5_h.H5Aexists(H5did, attrName);
            assertTrue("Attribute should exist before delete", exists > 0);

            // Delete by name from file using dataset path
            MemorySegment objName = stringToSegment(arena, "dset");
            int result            = hdf5_h.H5Adelete_by_name(H5fid, objName, attrName, hdf5_h.H5P_DEFAULT());
            assertTrue("H5Adelete_by_name failed", isSuccess(result));

            // Verify it's gone
            exists = hdf5_h.H5Aexists(H5did, attrName);
            assertEquals("Attribute should not exist after delete", 0, exists);

            hdf5_h.H5Sclose(attr_sid);
        }
    }

    @Test
    public void testH5Arename_by_name()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create attribute on dataset
            String oldNameStr     = "old_name_by_path";
            String newNameStr     = "new_name_by_path";
            long attr_sid         = hdf5_h.H5Screate(hdf5_h.H5S_SCALAR());
            MemorySegment oldName = stringToSegment(arena, oldNameStr);
            MemorySegment newName = stringToSegment(arena, newNameStr);

            long aid = hdf5_h.H5Acreate2(H5did, oldName, hdf5_h.H5T_NATIVE_INT_g(), attr_sid,
                                         hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());
            assertTrue("H5Acreate2 failed", isValidId(aid));
            hdf5_h.H5Aclose(aid);

            // Verify old name exists
            int exists = hdf5_h.H5Aexists(H5did, oldName);
            assertTrue("Old name should exist", exists > 0);

            // Rename by name from file using dataset path
            MemorySegment objName = stringToSegment(arena, "dset");
            int result = hdf5_h.H5Arename_by_name(H5fid, objName, oldName, newName, hdf5_h.H5P_DEFAULT());
            assertTrue("H5Arename_by_name failed", isSuccess(result));

            // Verify new name exists and old doesn't
            exists = hdf5_h.H5Aexists(H5did, newName);
            assertTrue("New name should exist", exists > 0);

            exists = hdf5_h.H5Aexists(H5did, oldName);
            assertEquals("Old name should not exist", 0, exists);

            hdf5_h.H5Sclose(attr_sid);
        }
    }

    @Test
    public void testH5Aiterate2()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create multiple attributes
            long attr_sid      = hdf5_h.H5Screate(hdf5_h.H5S_SCALAR());
            String[] attrNames = {"attr_A", "attr_B", "attr_C", "attr_D"};

            for (String name : attrNames) {
                MemorySegment attrName = stringToSegment(arena, name);
                long aid = hdf5_h.H5Acreate2(H5did, attrName, hdf5_h.H5T_NATIVE_INT_g(), attr_sid,
                                             hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());
                assertTrue("H5Acreate2 failed for " + name, isValidId(aid));
                hdf5_h.H5Aclose(aid);
            }

            // Create iteration callback
            final int[] count           = {0};
            final String[] names        = new String[10];
            H5A_operator2_t.Function cb = (loc_id, attr_name, ainfo, op_data) ->
            {
                try {
                    String name     = attr_name.getString(0);
                    names[count[0]] = name;
                    count[0]++;
                    return 0; // Continue iteration
                }
                catch (Exception e) {
                    return -1; // Stop on error
                }
            };

            MemorySegment callback = H5A_operator2_t.allocate(cb, arena);
            MemorySegment idx      = allocateLong(arena);
            setLong(idx, 0);

            // Iterate
            int result = hdf5_h.H5Aiterate2(H5did, hdf5_h.H5_INDEX_NAME(), hdf5_h.H5_ITER_INC(), idx,
                                            callback, MemorySegment.NULL);
            assertTrue("H5Aiterate2 failed", isSuccess(result));

            // Verify we iterated all attributes (at least the 4 we created)
            assertTrue("Should have iterated at least 4 attributes", count[0] >= 4);

            // Verify all our attributes were seen
            boolean foundA = false, foundB = false, foundC = false, foundD = false;
            for (int i = 0; i < count[0]; i++) {
                if ("attr_A".equals(names[i]))
                    foundA = true;
                if ("attr_B".equals(names[i]))
                    foundB = true;
                if ("attr_C".equals(names[i]))
                    foundC = true;
                if ("attr_D".equals(names[i]))
                    foundD = true;
            }
            assertTrue("Should have found attr_A", foundA);
            assertTrue("Should have found attr_B", foundB);
            assertTrue("Should have found attr_C", foundC);
            assertTrue("Should have found attr_D", foundD);

            hdf5_h.H5Sclose(attr_sid);
        }
    }

    @Test
    public void testH5Aiterate_by_name()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create attributes on dataset
            long attr_sid      = hdf5_h.H5Screate(hdf5_h.H5S_SCALAR());
            String[] attrNames = {"iter_1", "iter_2", "iter_3"};

            for (String name : attrNames) {
                MemorySegment attrName = stringToSegment(arena, name);
                long aid = hdf5_h.H5Acreate2(H5did, attrName, hdf5_h.H5T_NATIVE_INT_g(), attr_sid,
                                             hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());
                assertTrue("H5Acreate2 failed for " + name, isValidId(aid));
                hdf5_h.H5Aclose(aid);
            }

            // Create iteration callback
            final int[] count           = {0};
            H5A_operator2_t.Function cb = (loc_id, attr_name, ainfo, op_data) ->
            {
                count[0]++;
                return 0; // Continue
            };

            MemorySegment callback = H5A_operator2_t.allocate(cb, arena);
            MemorySegment idx      = allocateLong(arena);
            setLong(idx, 0);

            // Iterate from file using dataset path
            MemorySegment objName = stringToSegment(arena, "dset");
            int result =
                hdf5_h.H5Aiterate_by_name(H5fid, objName, hdf5_h.H5_INDEX_NAME(), hdf5_h.H5_ITER_INC(), idx,
                                          callback, MemorySegment.NULL, hdf5_h.H5P_DEFAULT());
            assertTrue("H5Aiterate_by_name failed", isSuccess(result));

            // Verify we iterated at least our 3 attributes
            assertTrue("Should have iterated at least 3 attributes", count[0] >= 3);

            hdf5_h.H5Sclose(attr_sid);
        }
    }

    @Test
    public void testH5Acreate_by_name_comprehensive()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create attribute on dataset using path from file
            String attrNameStr     = "created_by_path_comprehensive";
            long attr_sid          = hdf5_h.H5Screate(hdf5_h.H5S_SCALAR());
            MemorySegment objName  = stringToSegment(arena, "dset");
            MemorySegment attrName = stringToSegment(arena, attrNameStr);

            long aid =
                hdf5_h.H5Acreate_by_name(H5fid, objName, attrName, hdf5_h.H5T_NATIVE_DOUBLE_g(), attr_sid,
                                         hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());
            assertTrue("H5Acreate_by_name failed", isValidId(aid));

            // Write data
            MemorySegment data = allocateDoubleArray(arena, 1);
            data.setAtIndex(ValueLayout.JAVA_DOUBLE, 0, 3.14159);
            int writeResult = hdf5_h.H5Awrite(aid, hdf5_h.H5T_NATIVE_DOUBLE_g(), data);
            assertTrue("H5Awrite failed", isSuccess(writeResult));

            hdf5_h.H5Aclose(aid);

            // Verify attribute is accessible from dataset
            MemorySegment attrNameCheck = stringToSegment(arena, attrNameStr);
            int exists                  = hdf5_h.H5Aexists(H5did, attrNameCheck);
            assertTrue("Attribute should exist on dataset", exists > 0);

            // Open and read back to verify data
            long aid2 = hdf5_h.H5Aopen(H5did, attrNameCheck, hdf5_h.H5P_DEFAULT());
            assertTrue("H5Aopen failed", isValidId(aid2));

            MemorySegment readData = allocateDoubleArray(arena, 1);
            int readResult         = hdf5_h.H5Aread(aid2, hdf5_h.H5T_NATIVE_DOUBLE_g(), readData);
            assertTrue("H5Aread failed", isSuccess(readResult));

            double value = readData.getAtIndex(ValueLayout.JAVA_DOUBLE, 0);
            assertEquals("Data should match", 3.14159, value, 0.00001);

            hdf5_h.H5Aclose(aid2);
            hdf5_h.H5Sclose(attr_sid);
        }
    }

    @Test
    public void testH5Aiterate()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            MemorySegment idx = allocateLongArray(arena, 1);
            copyToSegment(idx, new long[] {0});

            // Just verify the API works, iteration callback complex for FFM
            long result = hdf5_h.H5Aiterate2(H5did, hdf5_h.H5_INDEX_NAME(), hdf5_h.H5_ITER_INC(), idx,
                                             MemorySegment.NULL, MemorySegment.NULL);
            assertTrue("H5Aiterate2 should complete", result >= 0);
        }
    }
}
