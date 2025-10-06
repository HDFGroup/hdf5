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

import static jtest.FfmTestSupport.*;
import static org.junit.Assert.*;

import java.lang.foreign.Arena;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.ValueLayout;

import org.hdfgroup.javahdf5.hdf5_h;
import org.hdfgroup.javahdf5.hdf5_h_1;
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

    long H5fid = H5I_INVALID_HID();
    long H5did = H5I_INVALID_HID();
    long H5sid = H5I_INVALID_HID();
    long H5aid = H5I_INVALID_HID();

    @Before
    public void createH5file() throws Exception
    {
        try (Arena arena = Arena.ofConfined()) {
            // Create file
            MemorySegment fileName = stringToSegment(arena, H5_FILE);
            H5fid = hdf5_h_1.H5Fcreate(fileName, hdf5_h.H5F_ACC_TRUNC(), hdf5_h_1.H5P_DEFAULT(),
                                       hdf5_h_1.H5P_DEFAULT());
            assertTrue("H5Fcreate failed", isValidId(H5fid));

            // Create dataspace
            long[] dims              = {DIM_X, DIM_Y};
            MemorySegment dimsSegment = allocateLongArray(arena, RANK);
            copyToSegment(dimsSegment, dims);
            H5sid = hdf5_h_1.H5Screate_simple(RANK, dimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple failed", isValidId(H5sid));

            // Create dataset for attaching attributes
            MemorySegment dsetName = stringToSegment(arena, "dset");
            H5did = hdf5_h_1.H5Dcreate2(H5fid, dsetName, hdf5_h_1.H5T_NATIVE_INT_g(), H5sid,
                                        hdf5_h_1.H5P_DEFAULT(), hdf5_h_1.H5P_DEFAULT(),
                                        hdf5_h_1.H5P_DEFAULT());
            assertTrue("H5Dcreate2 failed", isValidId(H5did));
        }
    }

    @After
    public void deleteH5file() throws Exception
    {
        closeQuietly(H5aid, hdf5_h_1::H5Aclose);
        closeQuietly(H5did, hdf5_h_1::H5Dclose);
        closeQuietly(H5sid, hdf5_h_1::H5Sclose);
        closeQuietly(H5fid, hdf5_h_1::H5Fclose);

        H5aid = H5I_INVALID_HID();
        H5did = H5I_INVALID_HID();
        H5sid = H5I_INVALID_HID();
        H5fid = H5I_INVALID_HID();
    }

    static {
        try {
            System.loadLibrary("hdf5");
            hdf5_h_1.H5open();
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
            long attr_sid = hdf5_h_1.H5Screate(hdf5_h.H5S_SCALAR());
            assertTrue("H5Screate scalar failed", isValidId(attr_sid));

            // Create attribute
            MemorySegment attrName = stringToSegment(arena, "attr1");
            H5aid = hdf5_h_1.H5Acreate2(H5did, attrName, hdf5_h_1.H5T_NATIVE_INT_g(), attr_sid,
                                        hdf5_h_1.H5P_DEFAULT(), hdf5_h_1.H5P_DEFAULT());
            assertTrue("H5Acreate2 failed", isValidId(H5aid));

            hdf5_h_1.H5Sclose(attr_sid);
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

            long attr_sid = hdf5_h_1.H5Screate_simple(1, attrDimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple failed", isValidId(attr_sid));

            MemorySegment attrName = stringToSegment(arena, "int_array_attr");
            H5aid = hdf5_h_1.H5Acreate2(H5did, attrName, hdf5_h_1.H5T_NATIVE_INT_g(), attr_sid,
                                        hdf5_h_1.H5P_DEFAULT(), hdf5_h_1.H5P_DEFAULT());
            assertTrue("H5Acreate2 failed", isValidId(H5aid));

            // Write data
            int[] write_data              = {10, 20, 30};
            MemorySegment writeSegment    = allocateIntArray(arena, 3);
            copyToSegment(writeSegment, write_data);

            int result = hdf5_h_1.H5Awrite(H5aid, hdf5_h_1.H5T_NATIVE_INT_g(), writeSegment);
            assertTrue("H5Awrite failed", isSuccess(result));

            // Read back
            MemorySegment readSegment = allocateIntArray(arena, 3);
            result = hdf5_h_1.H5Aread(H5aid, hdf5_h_1.H5T_NATIVE_INT_g(), readSegment);
            assertTrue("H5Aread failed", isSuccess(result));

            // Verify
            int[] read_data = new int[3];
            copyFromSegment(readSegment, read_data);
            assertArrayEquals("Data mismatch", write_data, read_data);

            hdf5_h_1.H5Sclose(attr_sid);
        }
    }

    @Test
    public void testH5Aopen()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create attribute first
            long attr_sid           = hdf5_h_1.H5Screate(hdf5_h.H5S_SCALAR());
            MemorySegment attrName  = stringToSegment(arena, "test_attr");

            long aid = hdf5_h_1.H5Acreate2(H5did, attrName, hdf5_h_1.H5T_NATIVE_INT_g(), attr_sid,
                                           hdf5_h_1.H5P_DEFAULT(), hdf5_h_1.H5P_DEFAULT());
            assertTrue("H5Acreate2 failed", isValidId(aid));
            hdf5_h_1.H5Aclose(aid);

            // Open attribute
            H5aid = hdf5_h_1.H5Aopen(H5did, attrName, hdf5_h_1.H5P_DEFAULT());
            assertTrue("H5Aopen failed", isValidId(H5aid));

            hdf5_h_1.H5Sclose(attr_sid);
        }
    }

    @Test
    public void testH5Aclose()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long attr_sid          = hdf5_h_1.H5Screate(hdf5_h.H5S_SCALAR());
            MemorySegment attrName = stringToSegment(arena, "temp_attr");

            H5aid = hdf5_h_1.H5Acreate2(H5did, attrName, hdf5_h_1.H5T_NATIVE_INT_g(), attr_sid,
                                        hdf5_h_1.H5P_DEFAULT(), hdf5_h_1.H5P_DEFAULT());
            assertTrue("H5Acreate2 failed", isValidId(H5aid));

            int result = hdf5_h_1.H5Aclose(H5aid);
            assertTrue("H5Aclose failed", isSuccess(result));

            H5aid = H5I_INVALID_HID();
            hdf5_h_1.H5Sclose(attr_sid);
        }
    }

    @Test
    public void testH5Aget_name()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            String expectedName    = "my_attribute";
            long attr_sid          = hdf5_h_1.H5Screate(hdf5_h.H5S_SCALAR());
            MemorySegment attrName = stringToSegment(arena, expectedName);

            H5aid = hdf5_h_1.H5Acreate2(H5did, attrName, hdf5_h_1.H5T_NATIVE_INT_g(), attr_sid,
                                        hdf5_h_1.H5P_DEFAULT(), hdf5_h_1.H5P_DEFAULT());
            assertTrue("H5Acreate2 failed", isValidId(H5aid));

            // Get name size
            long nameSize = hdf5_h_1.H5Aget_name(H5aid, 0, MemorySegment.NULL);
            assertTrue("H5Aget_name size query failed", nameSize > 0);

            // Get name
            MemorySegment nameBuffer = arena.allocate(nameSize + 1);
            hdf5_h_1.H5Aget_name(H5aid, nameSize + 1, nameBuffer);

            String retrievedName = nameBuffer.getString(0);
            assertEquals("Attribute name mismatch", expectedName, retrievedName);

            hdf5_h_1.H5Sclose(attr_sid);
        }
    }

    @Test
    public void testH5Aget_space()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create attribute with specific dimensions
            long[] dims              = {5, 3};
            MemorySegment dimsSegment = allocateLongArray(arena, 2);
            copyToSegment(dimsSegment, dims);

            long attr_sid          = hdf5_h_1.H5Screate_simple(2, dimsSegment, MemorySegment.NULL);
            MemorySegment attrName = stringToSegment(arena, "array_attr");

            H5aid = hdf5_h_1.H5Acreate2(H5did, attrName, hdf5_h_1.H5T_NATIVE_INT_g(), attr_sid,
                                        hdf5_h_1.H5P_DEFAULT(), hdf5_h_1.H5P_DEFAULT());
            assertTrue("H5Acreate2 failed", isValidId(H5aid));

            // Get dataspace
            long retrieved_sid = hdf5_h_1.H5Aget_space(H5aid);
            assertTrue("H5Aget_space failed", isValidId(retrieved_sid));

            // Verify dimensions
            MemorySegment retrievedDims = allocateLongArray(arena, 2);
            hdf5_h_1.H5Sget_simple_extent_dims(retrieved_sid, retrievedDims, MemorySegment.NULL);

            long[] readDims = new long[2];
            copyFromSegment(retrievedDims, readDims);
            assertArrayEquals("Dimensions mismatch", dims, readDims);

            hdf5_h_1.H5Sclose(retrieved_sid);
            hdf5_h_1.H5Sclose(attr_sid);
        }
    }

    @Test
    public void testH5Aget_type()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long attr_sid          = hdf5_h_1.H5Screate(hdf5_h.H5S_SCALAR());
            MemorySegment attrName = stringToSegment(arena, "type_attr");

            H5aid = hdf5_h_1.H5Acreate2(H5did, attrName, hdf5_h_1.H5T_NATIVE_DOUBLE_g(), attr_sid,
                                        hdf5_h_1.H5P_DEFAULT(), hdf5_h_1.H5P_DEFAULT());
            assertTrue("H5Acreate2 failed", isValidId(H5aid));

            // Get type
            long retrieved_tid = hdf5_h_1.H5Aget_type(H5aid);
            assertTrue("H5Aget_type failed", isValidId(retrieved_tid));

            // Verify it's a double type
            int equal = hdf5_h_1.H5Tequal(retrieved_tid, hdf5_h_1.H5T_NATIVE_DOUBLE_g());
            assertTrue("Type should be H5T_NATIVE_DOUBLE", equal > 0);

            hdf5_h_1.H5Tclose(retrieved_tid);
            hdf5_h_1.H5Sclose(attr_sid);
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
            long attr_sid = hdf5_h_1.H5Screate(hdf5_h.H5S_SCALAR());
            long aid      = hdf5_h_1.H5Acreate2(H5did, existingName, hdf5_h_1.H5T_NATIVE_INT_g(), attr_sid,
                                           hdf5_h_1.H5P_DEFAULT(), hdf5_h_1.H5P_DEFAULT());
            assertTrue("H5Acreate2 failed", isValidId(aid));
            hdf5_h_1.H5Aclose(aid);

            // Check existing
            int exists = hdf5_h_1.H5Aexists(H5did, existingName);
            assertTrue("Attribute should exist", exists > 0);

            // Check non-existing
            exists = hdf5_h_1.H5Aexists(H5did, missingName);
            assertEquals("Attribute should not exist", 0, exists);

            hdf5_h_1.H5Sclose(attr_sid);
        }
    }

    @Test
    public void testH5Adelete()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            MemorySegment attrName = stringToSegment(arena, "deletable_attr");

            // Create attribute
            long attr_sid = hdf5_h_1.H5Screate(hdf5_h.H5S_SCALAR());
            long aid      = hdf5_h_1.H5Acreate2(H5did, attrName, hdf5_h_1.H5T_NATIVE_INT_g(), attr_sid,
                                           hdf5_h_1.H5P_DEFAULT(), hdf5_h_1.H5P_DEFAULT());
            assertTrue("H5Acreate2 failed", isValidId(aid));
            hdf5_h_1.H5Aclose(aid);

            // Verify it exists
            int exists = hdf5_h_1.H5Aexists(H5did, attrName);
            assertTrue("Attribute should exist before delete", exists > 0);

            // Delete
            int result = hdf5_h_1.H5Adelete(H5did, attrName);
            assertTrue("H5Adelete failed", isSuccess(result));

            // Verify it's gone
            exists = hdf5_h_1.H5Aexists(H5did, attrName);
            assertEquals("Attribute should not exist after delete", 0, exists);

            hdf5_h_1.H5Sclose(attr_sid);
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

            long attr_sid          = hdf5_h_1.H5Screate_simple(1, attrDimsSegment, MemorySegment.NULL);
            MemorySegment attrName = stringToSegment(arena, "storage_attr");

            H5aid = hdf5_h_1.H5Acreate2(H5did, attrName, hdf5_h_1.H5T_NATIVE_INT_g(), attr_sid,
                                        hdf5_h_1.H5P_DEFAULT(), hdf5_h_1.H5P_DEFAULT());
            assertTrue("H5Acreate2 failed", isValidId(H5aid));

            // Write data
            int[] data                 = new int[10];
            MemorySegment dataSegment  = allocateIntArray(arena, 10);
            copyToSegment(dataSegment, data);
            hdf5_h_1.H5Awrite(H5aid, hdf5_h_1.H5T_NATIVE_INT_g(), dataSegment);

            // Get storage size
            long storage_size = hdf5_h_1.H5Aget_storage_size(H5aid);
            assertEquals("Storage size should be 10 * sizeof(int)", 40L, storage_size);

            hdf5_h_1.H5Sclose(attr_sid);
        }
    }

    @Test
    public void testH5Awrite_readStr()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            String testString = "Hello HDF5 Attributes!";

            // Create string type
            long str_tid = hdf5_h_1.H5Tcopy(hdf5_h_1.H5T_C_S1_g());
            assertTrue("H5Tcopy failed", isValidId(str_tid));

            hdf5_h_1.H5Tset_size(str_tid, testString.length() + 1);
            hdf5_h_1.H5Tset_strpad(str_tid, hdf5_h.H5T_STR_NULLTERM());

            // Create attribute
            long attr_sid          = hdf5_h_1.H5Screate(hdf5_h.H5S_SCALAR());
            MemorySegment attrName = stringToSegment(arena, "str_attr");

            H5aid = hdf5_h_1.H5Acreate2(H5did, attrName, str_tid, attr_sid, hdf5_h_1.H5P_DEFAULT(),
                                        hdf5_h_1.H5P_DEFAULT());
            assertTrue("H5Acreate2 failed", isValidId(H5aid));

            // Write string
            MemorySegment writeData = stringToSegment(arena, testString);
            int writeResult         = hdf5_h_1.H5Awrite(H5aid, str_tid, writeData);
            assertTrue("H5Awrite failed", isSuccess(writeResult));

            // Read string back
            MemorySegment readData = arena.allocate(testString.length() + 1);
            int readResult         = hdf5_h_1.H5Aread(H5aid, str_tid, readData);
            assertTrue("H5Aread failed", isSuccess(readResult));

            String retrievedString = readData.getString(0);
            assertEquals("String mismatch", testString, retrievedString);

            hdf5_h_1.H5Tclose(str_tid);
            hdf5_h_1.H5Sclose(attr_sid);
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
            long attr_sid = hdf5_h_1.H5Screate(hdf5_h.H5S_SCALAR());
            long aid      = hdf5_h_1.H5Acreate2(H5did, oldName, hdf5_h_1.H5T_NATIVE_INT_g(), attr_sid,
                                           hdf5_h_1.H5P_DEFAULT(), hdf5_h_1.H5P_DEFAULT());
            assertTrue("H5Acreate2 failed", isValidId(aid));
            hdf5_h_1.H5Aclose(aid);

            // Verify old name exists
            int exists = hdf5_h_1.H5Aexists(H5did, oldName);
            assertTrue("Old name should exist", exists > 0);

            // Rename
            int result = hdf5_h_1.H5Arename(H5did, oldName, newName);
            assertTrue("H5Arename failed", isSuccess(result));

            // Verify new name exists and old doesn't
            exists = hdf5_h_1.H5Aexists(H5did, newName);
            assertTrue("New name should exist", exists > 0);

            exists = hdf5_h_1.H5Aexists(H5did, oldName);
            assertEquals("Old name should not exist", 0, exists);

            hdf5_h_1.H5Sclose(attr_sid);
        }
    }

    @Test
    public void testH5Aget_num_attrs()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create 3 attributes
            long attr_sid = hdf5_h_1.H5Screate(hdf5_h.H5S_SCALAR());

            for (int i = 0; i < 3; i++) {
                MemorySegment attrName = stringToSegment(arena, "attr_" + i);
                long aid               = hdf5_h_1.H5Acreate2(H5did, attrName, hdf5_h_1.H5T_NATIVE_INT_g(),
                                               attr_sid, hdf5_h_1.H5P_DEFAULT(), hdf5_h_1.H5P_DEFAULT());
                assertTrue("H5Acreate2 failed for attr_" + i, isValidId(aid));
                hdf5_h_1.H5Aclose(aid);
            }

            // Get number of attributes
            int num_attrs = hdf5_h_1.H5Aget_num_attrs(H5did);
            assertTrue("Should have at least 3 attributes", num_attrs >= 3);

            hdf5_h_1.H5Sclose(attr_sid);
        }
    }
}
