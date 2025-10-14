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
import org.junit.Ignore;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

/**
 * FFM-only tests for HDF5 Datatype (H5T) operations.
 *
 * This test class uses direct FFM bindings without the hdf.hdf5lib wrapper layer.
 */
public class TestH5Tffm {
    @Rule
    public TestName testname = new TestName();

    static
    {
        // Initialize FFM library by calling H5open()
        // This ensures global type variables are properly initialized
        try {
            hdf5_h_1.H5open();
        }
        catch (Exception e) {
            System.err.println("Warning: H5open() failed during FFM initialization: " + e);
        }
    }

    // Predefined datatype constants

    // Datatype classes

    // String padding

    long H5tid = hdf5_h.H5I_INVALID_HID();

    @After
    public void cleanup()
    {
        closeQuietly(H5tid, hdf5_h_1::H5Tclose);
        H5tid = hdf5_h.H5I_INVALID_HID();
        System.out.println();
    }

    @Test
    public void testH5Tcopy()
    {
        System.out.print(testname.getMethodName());

        H5tid = hdf5_h_1.H5Tcopy(hdf5_h_1.H5T_NATIVE_INT_g());
        assertTrue("H5Tcopy failed", isValidId(H5tid));
    }

    @Test
    public void testH5Tequal()
    {
        System.out.print(testname.getMethodName());
        long tid2 = hdf5_h.H5I_INVALID_HID();

        try {
            H5tid = hdf5_h_1.H5Tcopy(hdf5_h_1.H5T_NATIVE_INT_g());
            assertTrue("H5Tcopy failed", isValidId(H5tid));

            tid2 = hdf5_h_1.H5Tcopy(hdf5_h_1.H5T_NATIVE_INT_g());
            assertTrue("H5Tcopy failed", isValidId(tid2));

            int result = hdf5_h_1.H5Tequal(H5tid, tid2);
            assertTrue("Types should be equal", result > 0);

            // Compare with different type
            result = hdf5_h_1.H5Tequal(H5tid, hdf5_h_1.H5T_NATIVE_FLOAT_g());
            assertFalse("Types should not be equal", result > 0);
        }
        finally {
            closeQuietly(tid2, hdf5_h_1::H5Tclose);
        }
    }

    @Test
    public void testH5Tget_class()
    {
        System.out.print(testname.getMethodName());

        H5tid = hdf5_h_1.H5Tcopy(hdf5_h_1.H5T_NATIVE_INT_g());
        assertTrue("H5Tcopy failed", isValidId(H5tid));

        int tclass = hdf5_h_1.H5Tget_class(H5tid);
        assertEquals("Type class should be INTEGER", hdf5_h.H5T_INTEGER(), tclass);
    }

    @Test
    public void testH5Tget_size()
    {
        System.out.print(testname.getMethodName());

        H5tid = hdf5_h_1.H5Tcopy(hdf5_h_1.H5T_NATIVE_INT_g());
        assertTrue("H5Tcopy failed", isValidId(H5tid));

        long size = hdf5_h_1.H5Tget_size(H5tid);
        assertTrue("Type size should be > 0", size > 0);
        assertEquals("Native int should be 4 bytes", 4, size);
    }

    @Test
    public void testH5Tset_size()
    {
        System.out.print(testname.getMethodName());

        H5tid = hdf5_h_1.H5Tcopy(hdf5_h_1.H5T_C_S1_g());
        assertTrue("H5Tcopy failed", isValidId(H5tid));

        int result = hdf5_h_1.H5Tset_size(H5tid, 64);
        assertTrue("H5Tset_size failed", isSuccess(result));

        long size = hdf5_h_1.H5Tget_size(H5tid);
        assertEquals("Size should be 64", 64, size);
    }

    @Test
    public void testH5Tget_order()
    {
        System.out.print(testname.getMethodName());

        H5tid = hdf5_h_1.H5Tcopy(hdf5_h_1.H5T_STD_I32LE_g());
        assertTrue("H5Tcopy failed", isValidId(H5tid));

        int order = hdf5_h_1.H5Tget_order(H5tid);
        assertTrue("Byte order should be valid", order >= 0);
    }

    @Test
    public void testH5Tget_precision()
    {
        System.out.print(testname.getMethodName());

        H5tid = hdf5_h_1.H5Tcopy(hdf5_h_1.H5T_NATIVE_INT_g());
        assertTrue("H5Tcopy failed", isValidId(H5tid));

        long precision = hdf5_h_1.H5Tget_precision(H5tid);
        assertTrue("Precision should be > 0", precision > 0);
    }

    @Test
    public void testH5Tset_precision()
    {
        System.out.print(testname.getMethodName());

        H5tid = hdf5_h_1.H5Tcopy(hdf5_h_1.H5T_NATIVE_INT_g());
        assertTrue("H5Tcopy failed", isValidId(H5tid));

        int result = hdf5_h_1.H5Tset_precision(H5tid, 16);
        assertTrue("H5Tset_precision failed", isSuccess(result));

        long precision = hdf5_h_1.H5Tget_precision(H5tid);
        assertEquals("Precision should be 16", 16, precision);
    }

    @Test
    public void testH5Tget_strpad()
    {
        System.out.print(testname.getMethodName());

        H5tid = hdf5_h_1.H5Tcopy(hdf5_h_1.H5T_C_S1_g());
        assertTrue("H5Tcopy failed", isValidId(H5tid));

        int strpad = hdf5_h_1.H5Tget_strpad(H5tid);
        assertTrue("String padding should be valid", strpad >= 0);
    }

    @Test
    public void testH5Tcreate_compound()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create a compound type with int and double
            int compoundSize = 4 + 8; // sizeof(int) + sizeof(double)
            H5tid            = hdf5_h_1.H5Tcreate(hdf5_h.H5T_COMPOUND(), compoundSize);
            assertTrue("H5Tcreate failed", isValidId(H5tid));

            // Insert int member
            MemorySegment intNameSegment = stringToSegment(arena, "int_field");
            int result = hdf5_h_1.H5Tinsert(H5tid, intNameSegment, 0, hdf5_h_1.H5T_NATIVE_INT_g());
            assertTrue("H5Tinsert int failed", isSuccess(result));

            // Insert double member
            MemorySegment doubleNameSegment = stringToSegment(arena, "double_field");
            result = hdf5_h_1.H5Tinsert(H5tid, doubleNameSegment, 4, hdf5_h_1.H5T_NATIVE_DOUBLE_g());
            assertTrue("H5Tinsert double failed", isSuccess(result));

            // Verify it's a compound type
            int tclass = hdf5_h_1.H5Tget_class(H5tid);
            assertEquals("Type class should be COMPOUND", hdf5_h.H5T_COMPOUND(), tclass);

            // Verify number of members
            int nmembers = hdf5_h_1.H5Tget_nmembers(H5tid);
            assertEquals("Should have 2 members", 2, nmembers);
        }
    }

    @Test
    public void testH5Tget_nmembers()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            H5tid = hdf5_h_1.H5Tcreate(hdf5_h.H5T_COMPOUND(), 12);
            assertTrue("H5Tcreate failed", isValidId(H5tid));

            MemorySegment nameSegment = stringToSegment(arena, "field1");
            hdf5_h_1.H5Tinsert(H5tid, nameSegment, 0, hdf5_h_1.H5T_NATIVE_INT_g());

            int nmembers = hdf5_h_1.H5Tget_nmembers(H5tid);
            assertEquals("Should have 1 member", 1, nmembers);
        }
    }

    @Test
    public void testH5Tget_member_name()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            H5tid = hdf5_h_1.H5Tcreate(hdf5_h.H5T_COMPOUND(), 12);
            assertTrue("H5Tcreate failed", isValidId(H5tid));

            String fieldName          = "test_field";
            MemorySegment nameSegment = stringToSegment(arena, fieldName);
            hdf5_h_1.H5Tinsert(H5tid, nameSegment, 0, hdf5_h_1.H5T_NATIVE_INT_g());

            MemorySegment returnedName = hdf5_h_1.H5Tget_member_name(H5tid, 0);
            assertFalse("Returned name should not be null", returnedName.equals(MemorySegment.NULL));

            String memberName = returnedName.getString(0);
            assertEquals("Member name should match", fieldName, memberName);
        }
    }

    @Test
    public void testH5Tget_member_offset()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            H5tid = hdf5_h_1.H5Tcreate(hdf5_h.H5T_COMPOUND(), 12);
            assertTrue("H5Tcreate failed", isValidId(H5tid));

            long expectedOffset       = 4;
            MemorySegment nameSegment = stringToSegment(arena, "field");
            hdf5_h_1.H5Tinsert(H5tid, nameSegment, expectedOffset, hdf5_h_1.H5T_NATIVE_INT_g());

            long offset = hdf5_h_1.H5Tget_member_offset(H5tid, 0);
            assertEquals("Offset should match", expectedOffset, offset);
        }
    }

    @Test
    public void testH5Tget_member_type()
    {
        System.out.print(testname.getMethodName());
        long memberType = hdf5_h.H5I_INVALID_HID();

        try (Arena arena = Arena.ofConfined()) {
            H5tid = hdf5_h_1.H5Tcreate(hdf5_h.H5T_COMPOUND(), 12);
            assertTrue("H5Tcreate failed", isValidId(H5tid));

            MemorySegment nameSegment = stringToSegment(arena, "field");
            hdf5_h_1.H5Tinsert(H5tid, nameSegment, 0, hdf5_h_1.H5T_NATIVE_INT_g());

            memberType = hdf5_h_1.H5Tget_member_type(H5tid, 0);
            assertTrue("H5Tget_member_type failed", isValidId(memberType));

            // Verify it's an integer type
            int tclass = hdf5_h_1.H5Tget_class(memberType);
            assertEquals("Member type should be INTEGER", hdf5_h.H5T_INTEGER(), tclass);
        }
        finally {
            closeQuietly(memberType, hdf5_h_1::H5Tclose);
        }
    }

    @Test
    public void testH5Tarray_create()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create array type: int[3][4]
            int rank                  = 2;
            long[] dims               = {3, 4};
            MemorySegment dimsSegment = allocateLongArray(arena, rank);
            copyToSegment(dimsSegment, dims);

            H5tid = hdf5_h_1.H5Tarray_create2(hdf5_h_1.H5T_NATIVE_INT_g(), rank, dimsSegment);
            assertTrue("H5Tarray_create2 failed", isValidId(H5tid));

            // Verify it's an array type
            int tclass = hdf5_h_1.H5Tget_class(H5tid);
            assertEquals("Type class should be ARRAY", hdf5_h.H5T_ARRAY(), tclass);
        }
    }

    @Test
    public void testH5Tget_array_dims()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            int rank                  = 2;
            long[] expectedDims       = {3, 4};
            MemorySegment dimsSegment = allocateLongArray(arena, rank);
            copyToSegment(dimsSegment, expectedDims);

            H5tid = hdf5_h_1.H5Tarray_create2(hdf5_h_1.H5T_NATIVE_INT_g(), rank, dimsSegment);
            assertTrue("H5Tarray_create2 failed", isValidId(H5tid));

            MemorySegment returnedDimsSegment = allocateLongArray(arena, rank);
            int result                        = hdf5_h_1.H5Tget_array_dims2(H5tid, returnedDimsSegment);
            assertEquals("H5Tget_array_dims2 should return rank", rank, result);

            long[] returnedDims = new long[rank];
            copyFromSegment(returnedDimsSegment, returnedDims);
            assertArrayEquals("Array dimensions should match", expectedDims, returnedDims);
        }
    }

    @Test
    public void testH5Tenum_operations()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create enum type
            H5tid = hdf5_h_1.H5Tenum_create(hdf5_h_1.H5T_NATIVE_INT_g());
            assertTrue("H5Tenum_create failed", isValidId(H5tid));

            // Insert enum values
            MemorySegment redSegment      = stringToSegment(arena, "RED");
            MemorySegment redValueSegment = allocateInt(arena);
            setInt(redValueSegment, 0);
            int result = hdf5_h_1.H5Tenum_insert(H5tid, redSegment, redValueSegment);
            assertTrue("H5Tenum_insert RED failed", isSuccess(result));

            MemorySegment greenSegment      = stringToSegment(arena, "GREEN");
            MemorySegment greenValueSegment = allocateInt(arena);
            setInt(greenValueSegment, 1);
            result = hdf5_h_1.H5Tenum_insert(H5tid, greenSegment, greenValueSegment);
            assertTrue("H5Tenum_insert GREEN failed", isSuccess(result));

            MemorySegment blueSegment      = stringToSegment(arena, "BLUE");
            MemorySegment blueValueSegment = allocateInt(arena);
            setInt(blueValueSegment, 2);
            result = hdf5_h_1.H5Tenum_insert(H5tid, blueSegment, blueValueSegment);
            assertTrue("H5Tenum_insert BLUE failed", isSuccess(result));

            // Verify number of members
            int nmembers = hdf5_h_1.H5Tget_nmembers(H5tid);
            assertEquals("Should have 3 members", 3, nmembers);

            // Test H5Tenum_nameof - get name from value
            MemorySegment lookupValueSegment = allocateInt(arena);
            setInt(lookupValueSegment, 1);

            MemorySegment nameSegment = arena.allocate(64); // Allocate buffer for name
            int nameResult            = hdf5_h_1.H5Tenum_nameof(H5tid, lookupValueSegment, nameSegment, 64);
            assertTrue("H5Tenum_nameof failed", isSuccess(nameResult));

            String name = nameSegment.getString(0);
            assertEquals("Name should be GREEN", "GREEN", name);

            // Test H5Tenum_valueof - get value from name
            MemorySegment lookupNameSegment = stringToSegment(arena, "BLUE");
            MemorySegment valueSegment      = allocateInt(arena);
            result = hdf5_h_1.H5Tenum_valueof(H5tid, lookupNameSegment, valueSegment);
            assertTrue("H5Tenum_valueof failed", isSuccess(result));

            int value = getInt(valueSegment);
            assertEquals("Value should be 2", 2, value);

            // Verify it's an enum type
            int tclass = hdf5_h_1.H5Tget_class(H5tid);
            assertEquals("Type class should be ENUM", hdf5_h.H5T_ENUM(), tclass);
        }
    }

    @Test
    public void testH5Tis_variable_str()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create fixed-length string type
            H5tid = hdf5_h_1.H5Tcopy(hdf5_h_1.H5T_C_S1_g());
            assertTrue("H5Tcopy failed", isValidId(H5tid));

            hdf5_h_1.H5Tset_size(H5tid, 10);

            int result = hdf5_h_1.H5Tis_variable_str(H5tid);
            assertFalse("Fixed-length string should not be variable", result > 0);

            // Close and create variable-length string type
            hdf5_h_1.H5Tclose(H5tid);

            H5tid = hdf5_h_1.H5Tcopy(hdf5_h_1.H5T_C_S1_g());
            assertTrue("H5Tcopy failed", isValidId(H5tid));

            hdf5_h_1.H5Tset_size(H5tid, -1); // H5T_VARIABLE

            result = hdf5_h_1.H5Tis_variable_str(H5tid);
            assertTrue("Variable-length string should be variable", result > 0);
        }
    }

    @Test
    public void testH5Tget_cset()
    {
        System.out.print(testname.getMethodName());

        H5tid = hdf5_h_1.H5Tcopy(hdf5_h_1.H5T_C_S1_g());
        assertTrue("H5Tcopy failed", isValidId(H5tid));

        int cset = hdf5_h_1.H5Tget_cset(H5tid);
        assertTrue("Character set should be valid", cset >= 0);
        // H5T_CSET_ASCII = 0
        assertEquals("Default character set should be ASCII", 0, cset);
    }

    @Test
    public void testH5Tclose()
    {
        System.out.print(testname.getMethodName());

        H5tid = hdf5_h_1.H5Tcopy(hdf5_h_1.H5T_NATIVE_INT_g());
        assertTrue("H5Tcopy failed", isValidId(H5tid));

        int result = hdf5_h_1.H5Tclose(H5tid);
        assertTrue("H5Tclose failed", isSuccess(result));
        H5tid = hdf5_h.H5I_INVALID_HID();
    }

    @Test
    public void testH5Tvlen_create()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create variable-length type of integers
            H5tid = hdf5_h_1.H5Tvlen_create(hdf5_h_1.H5T_NATIVE_INT_g());
            assertTrue("H5Tvlen_create failed", isValidId(H5tid));

            // Verify it's a variable-length type
            int tclass = hdf5_h_1.H5Tget_class(H5tid);
            assertEquals("Should be H5T_VLEN class", hdf5_h.H5T_VLEN(), tclass);

            // Get the base type
            long base_type = hdf5_h_1.H5Tget_super(H5tid);
            assertTrue("H5Tget_super should return valid type", isValidId(base_type));

            // Verify base type is integer
            int equal = hdf5_h_1.H5Tequal(base_type, hdf5_h_1.H5T_NATIVE_INT_g());
            assertTrue("Base type should be H5T_NATIVE_INT", equal > 0);

            hdf5_h_1.H5Tclose(base_type);
        }
    }

    @Test
    public void testH5Topaque_operations()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create opaque type with 16 bytes
            long size = 16;
            H5tid     = hdf5_h_1.H5Tcreate(hdf5_h.H5T_OPAQUE(), size);
            assertTrue("H5Tcreate opaque failed", isValidId(H5tid));

            // Verify it's opaque
            int tclass = hdf5_h_1.H5Tget_class(H5tid);
            assertEquals("Should be H5T_OPAQUE class", hdf5_h.H5T_OPAQUE(), tclass);

            // Set tag for opaque type
            String tag               = "16-byte opaque data";
            MemorySegment tagSegment = stringToSegment(arena, tag);
            int result               = hdf5_h_1.H5Tset_tag(H5tid, tagSegment);
            assertTrue("H5Tset_tag failed", isSuccess(result));

            // Get tag back
            MemorySegment outTag = hdf5_h_1.H5Tget_tag(H5tid);
            assertFalse("H5Tget_tag should return valid pointer", outTag.address() == 0);

            String retrievedTag = outTag.getString(0);
            assertEquals("Tag should match", tag, retrievedTag);

            // Verify size
            long retrievedSize = hdf5_h_1.H5Tget_size(H5tid);
            assertEquals("Size should be 16", size, retrievedSize);
        }
    }

    @Test
    public void testH5Tget_sign_set_sign()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create integer type
            H5tid = hdf5_h_1.H5Tcopy(hdf5_h_1.H5T_NATIVE_INT_g());
            assertTrue("H5Tcopy failed", isValidId(H5tid));

            // Get current sign
            int sign = hdf5_h_1.H5Tget_sign(H5tid);
            assertTrue("H5Tget_sign should succeed", sign >= 0);

            // Set to unsigned
            int result = hdf5_h_1.H5Tset_sign(H5tid, hdf5_h.H5T_SGN_NONE());
            assertTrue("H5Tset_sign failed", isSuccess(result));

            // Verify sign changed
            int newSign = hdf5_h_1.H5Tget_sign(H5tid);
            assertEquals("Sign should be H5T_SGN_NONE", hdf5_h.H5T_SGN_NONE(), newSign);
        }
    }

    @Test
    public void testH5Tget_offset_set_offset()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create integer type
            H5tid = hdf5_h_1.H5Tcopy(hdf5_h_1.H5T_NATIVE_INT_g());
            assertTrue("H5Tcopy failed", isValidId(H5tid));

            // Get current offset
            long offset = hdf5_h_1.H5Tget_offset(H5tid);
            assertTrue("H5Tget_offset should succeed", offset >= 0);

            // Set new offset (shift by 2 bits)
            long newOffset = 2;
            int result     = hdf5_h_1.H5Tset_offset(H5tid, newOffset);
            assertTrue("H5Tset_offset failed", isSuccess(result));

            // Verify offset changed
            long retrievedOffset = hdf5_h_1.H5Tget_offset(H5tid);
            assertEquals("Offset should be 2", newOffset, retrievedOffset);
        }
    }

    @Test
    public void testH5Tget_pad_set_pad()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create integer type
            H5tid = hdf5_h_1.H5Tcopy(hdf5_h_1.H5T_NATIVE_INT_g());
            assertTrue("H5Tcopy failed", isValidId(H5tid));

            // Get current padding
            MemorySegment lsbSegment = allocateInt(arena);
            MemorySegment msbSegment = allocateInt(arena);
            int result               = hdf5_h_1.H5Tget_pad(H5tid, lsbSegment, msbSegment);
            assertTrue("H5Tget_pad failed", isSuccess(result));

            // Set new padding (both to zero)
            result = hdf5_h_1.H5Tset_pad(H5tid, hdf5_h.H5T_PAD_ZERO(), hdf5_h.H5T_PAD_ZERO());
            assertTrue("H5Tset_pad failed", isSuccess(result));

            // Verify padding changed
            MemorySegment newLsbSegment = allocateInt(arena);
            MemorySegment newMsbSegment = allocateInt(arena);
            result                      = hdf5_h_1.H5Tget_pad(H5tid, newLsbSegment, newMsbSegment);
            assertTrue("H5Tget_pad failed", isSuccess(result));

            assertEquals("LSB padding should be ZERO", hdf5_h.H5T_PAD_ZERO(), getInt(newLsbSegment));
            assertEquals("MSB padding should be ZERO", hdf5_h.H5T_PAD_ZERO(), getInt(newMsbSegment));
        }
    }

    @Test
    public void testH5Tconvert_basic()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create buffer with int values
            int numElements = 5;
            int[] intData   = {1, 2, 3, 4, 5};

            // Allocate buffer and copy int data
            MemorySegment buffer = arena.allocate(numElements * 8); // Enough for doubles
            for (int i = 0; i < numElements; i++) {
                buffer.setAtIndex(java.lang.foreign.ValueLayout.JAVA_INT, i, intData[i]);
            }

            // Convert int to double
            long srcType = hdf5_h_1.H5T_NATIVE_INT_g();
            long dstType = hdf5_h_1.H5T_NATIVE_DOUBLE_g();
            int result   = hdf5_h_1.H5Tconvert(srcType, dstType, numElements, buffer, MemorySegment.NULL,
                                               hdf5_h.H5P_DEFAULT());
            assertTrue("H5Tconvert failed", isSuccess(result));

            // Verify first converted value
            double convertedValue = buffer.getAtIndex(java.lang.foreign.ValueLayout.JAVA_DOUBLE, 0);
            assertEquals("First value should be 1.0", 1.0, convertedValue, 0.001);
        }
    }

    @Test
    public void testH5Tconvert_int_to_float()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create buffer with int values
            int numElements = 3;
            int[] intData   = {10, 20, 30};

            // Allocate separate buffers for in-place conversion
            MemorySegment buffer = arena.allocate(numElements * 4); // 4 bytes per int/float
            for (int i = 0; i < numElements; i++) {
                buffer.setAtIndex(java.lang.foreign.ValueLayout.JAVA_INT, i, intData[i]);
            }

            // Convert int to float in-place
            int result = hdf5_h_1.H5Tconvert(hdf5_h_1.H5T_NATIVE_INT_g(), hdf5_h_1.H5T_NATIVE_FLOAT_g(),
                                             numElements, buffer, MemorySegment.NULL, hdf5_h.H5P_DEFAULT());
            assertTrue("H5Tconvert failed", isSuccess(result));

            // Verify converted values
            float val0 = buffer.getAtIndex(java.lang.foreign.ValueLayout.JAVA_FLOAT, 0);
            float val1 = buffer.getAtIndex(java.lang.foreign.ValueLayout.JAVA_FLOAT, 1);
            float val2 = buffer.getAtIndex(java.lang.foreign.ValueLayout.JAVA_FLOAT, 2);

            assertEquals("First value should be 10.0", 10.0f, val0, 0.001f);
            assertEquals("Second value should be 20.0", 20.0f, val1, 0.001f);
            assertEquals("Third value should be 30.0", 30.0f, val2, 0.001f);
        }
    }

    @Test
    public void testH5Treclaim_with_vlen_string()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create variable-length string type
            long strType = hdf5_h_1.H5Tcopy(hdf5_h_1.H5T_C_S1_g());
            assertTrue("H5Tcopy failed", isValidId(strType));

            int result = hdf5_h_1.H5Tset_size(strType, -1); // H5T_VARIABLE
            assertTrue("H5Tset_size failed", isSuccess(result));

            // Create simple 1D dataspace with 1 element
            long[] dimsArray   = {1};
            MemorySegment dims = allocateLongArray(arena, 1);
            copyToSegment(dims, dimsArray);
            long space = hdf5_h_1.H5Screate_simple(1, dims, MemorySegment.NULL);
            assertTrue("H5Screate_simple failed", isValidId(space));

            // Allocate buffer for pointer to string
            MemorySegment buffer = arena.allocate(8); // Pointer size

            // Set to NULL initially (nothing to reclaim, but tests the API)
            buffer.set(java.lang.foreign.ValueLayout.ADDRESS, 0, MemorySegment.NULL);

            // Test H5Treclaim - should succeed even with NULL pointer
            result = hdf5_h_1.H5Treclaim(strType, space, hdf5_h.H5P_DEFAULT(), buffer);
            assertTrue("H5Treclaim should succeed", isSuccess(result));

            // Cleanup
            hdf5_h.H5Sclose(space);
            hdf5_h_1.H5Tclose(strType);
        }
    }

    @Test
    public void testH5Tfind_conversion_path()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Try to find conversion path from int to float
            MemorySegment pcdata = arena.allocate(8); // Pointer to H5T_cdata_t*
            pcdata.set(java.lang.foreign.ValueLayout.ADDRESS, 0, MemorySegment.NULL);

            MemorySegment convFunc =
                hdf5_h.H5Tfind(hdf5_h_1.H5T_NATIVE_INT_g(), hdf5_h_1.H5T_NATIVE_FLOAT_g(), pcdata);

            // H5Tfind returns function pointer (can be NULL if no conversion exists)
            // For native types, conversion should exist
            assertFalse("Conversion function should be found", convFunc.equals(MemorySegment.NULL));
        }
    }

    @Test
    public void testH5Tfind_same_type()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Find conversion path from type to itself (should be no-op conversion)
            MemorySegment pcdata = arena.allocate(8);
            pcdata.set(java.lang.foreign.ValueLayout.ADDRESS, 0, MemorySegment.NULL);

            MemorySegment convFunc =
                hdf5_h.H5Tfind(hdf5_h_1.H5T_NATIVE_INT_g(), hdf5_h_1.H5T_NATIVE_INT_g(), pcdata);

            // Conversion from type to itself should exist (no-op)
            assertFalse("No-op conversion should be found", convFunc.equals(MemorySegment.NULL));
        }
    }

    @Test
    public void testH5Tget_fields()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create a copy of a floating point type
            long floatType = hdf5_h_1.H5Tcopy(hdf5_h_1.H5T_IEEE_F64LE_g());
            assertTrue("H5Tcopy failed", isValidId(floatType));

            // Get field positions for floating point type
            MemorySegment spos  = allocateLongArray(arena, 1); // sign position
            MemorySegment epos  = allocateLongArray(arena, 1); // exponent position
            MemorySegment esize = allocateLongArray(arena, 1); // exponent size
            MemorySegment mpos  = allocateLongArray(arena, 1); // mantissa position
            MemorySegment msize = allocateLongArray(arena, 1); // mantissa size

            int result = hdf5_h_1.H5Tget_fields(floatType, spos, epos, esize, mpos, msize);
            assertTrue("H5Tget_fields failed", isSuccess(result));

            // Verify we got valid values (all should be >= 0)
            long sposVal  = getLong(spos);
            long eposVal  = getLong(epos);
            long esizeVal = getLong(esize);
            long mposVal  = getLong(mpos);
            long msizeVal = getLong(msize);

            assertTrue("Sign position should be valid", sposVal >= 0);
            assertTrue("Exponent position should be valid", eposVal >= 0);
            assertTrue("Exponent size should be > 0", esizeVal > 0);
            assertTrue("Mantissa position should be valid", mposVal >= 0);
            assertTrue("Mantissa size should be > 0", msizeVal > 0);

            hdf5_h_1.H5Tclose(floatType);
        }
    }

    @Test
    public void testH5Tget_ebias()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Get exponent bias for double
            long floatType = hdf5_h_1.H5Tcopy(hdf5_h_1.H5T_IEEE_F64LE_g());
            assertTrue("H5Tcopy failed", isValidId(floatType));

            long ebias = hdf5_h_1.H5Tget_ebias(floatType);
            assertTrue("Exponent bias should be > 0", ebias > 0);

            // For IEEE 754 double, exponent bias is typically 1023
            // We won't test exact value as it's platform-dependent, but it should be reasonable
            assertTrue("Exponent bias should be reasonable", ebias < 10000);

            hdf5_h_1.H5Tclose(floatType);
        }
    }

    @Test
    public void testH5Tget_norm()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Get normalization type for floating point
            long floatType = hdf5_h_1.H5Tcopy(hdf5_h_1.H5T_NATIVE_FLOAT_g());
            assertTrue("H5Tcopy failed", isValidId(floatType));

            int norm = hdf5_h_1.H5Tget_norm(floatType);
            assertTrue("Normalization should be valid", norm >= 0);

            // Typical normalization types: IMPLIED (0), MSBSET (1), NONE (2)
            assertTrue("Normalization should be in valid range", norm <= 2);

            hdf5_h_1.H5Tclose(floatType);
        }
    }

    @Test
    public void testH5Tget_inpad()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Get internal padding type for floating point
            long floatType = hdf5_h_1.H5Tcopy(hdf5_h_1.H5T_NATIVE_DOUBLE_g());
            assertTrue("H5Tcopy failed", isValidId(floatType));

            int inpad = hdf5_h_1.H5Tget_inpad(floatType);
            assertTrue("Internal padding should be valid", inpad >= 0);

            // Padding types: ZERO (0), ONE (1), BACKGROUND (2)
            assertTrue("Internal padding should be in valid range", inpad <= 2);

            hdf5_h_1.H5Tclose(floatType);
        }
    }

    @Test
    public void testH5Tset_fields_and_ebias()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create a custom floating point type
            long floatType = hdf5_h_1.H5Tcopy(hdf5_h_1.H5T_IEEE_F32LE_g());
            assertTrue("H5Tcopy failed", isValidId(floatType));

            // Set custom field layout
            // For 32-bit float: sign(1) + exponent(8) + mantissa(23) = 32 bits
            long spos  = 31; // Sign at bit 31
            long epos  = 23; // Exponent starts at bit 23
            long esize = 8;  // Exponent is 8 bits
            long mpos  = 0;  // Mantissa starts at bit 0
            long msize = 23; // Mantissa is 23 bits

            int result = hdf5_h_1.H5Tset_fields(floatType, spos, epos, esize, mpos, msize);
            assertTrue("H5Tset_fields failed", isSuccess(result));

            // Set exponent bias (for 8-bit exponent, typical bias is 127)
            result = hdf5_h_1.H5Tset_ebias(floatType, 127);
            assertTrue("H5Tset_ebias failed", isSuccess(result));

            // Verify the settings
            long retrievedEbias = hdf5_h_1.H5Tget_ebias(floatType);
            assertEquals("Exponent bias should match", 127, retrievedEbias);

            hdf5_h_1.H5Tclose(floatType);
        }
    }

    // ============================================================================
    // H5T Array Datatype Tests
    // ============================================================================

    @Test
    public void testH5Tarray_create_1D()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create 1D array of integers [10]
            MemorySegment dims = arena.allocate(hdf5_h.C_LONG, 10);

            H5tid = hdf5_h_1.H5Tarray_create2(hdf5_h_1.H5T_NATIVE_INT_g(), 1, dims);
            assertTrue("H5Tarray_create2 failed", isValidId(H5tid));

            // Verify it's an array type
            int tclass = hdf5_h_1.H5Tget_class(H5tid);
            assertEquals("Should be array type", hdf5_h.H5T_ARRAY(), tclass);

            // Verify dimensions
            int ndims = hdf5_h_1.H5Tget_array_ndims(H5tid);
            assertEquals("Should be 1D array", 1, ndims);

            MemorySegment retrievedDims = arena.allocate(hdf5_h.C_LONG, 1);
            int result                  = hdf5_h_1.H5Tget_array_dims2(H5tid, retrievedDims);
            assertEquals("H5Tget_array_dims2 should succeed", 1, result);
            assertEquals("Dimension should be 10", 10, retrievedDims.get(hdf5_h.C_LONG, 0));
        }
    }

    @Test
    public void testH5Tarray_create_2D()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create 2D array of floats [3][4]
            MemorySegment dims = arena.allocateFrom(hdf5_h.C_LONG, 3, 4);

            H5tid = hdf5_h_1.H5Tarray_create2(hdf5_h_1.H5T_NATIVE_FLOAT_g(), 2, dims);
            assertTrue("H5Tarray_create2 failed", isValidId(H5tid));

            // Verify dimensions
            int ndims = hdf5_h_1.H5Tget_array_ndims(H5tid);
            assertEquals("Should be 2D array", 2, ndims);

            MemorySegment retrievedDims = arena.allocate(hdf5_h.C_LONG, 2);
            hdf5_h_1.H5Tget_array_dims2(H5tid, retrievedDims);
            assertEquals("First dimension should be 3", 3, retrievedDims.getAtIndex(hdf5_h.C_LONG, 0));
            assertEquals("Second dimension should be 4", 4, retrievedDims.getAtIndex(hdf5_h.C_LONG, 1));
        }
    }

    @Test
    public void testH5Tarray_create_3D()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create 3D array of doubles [2][3][4]
            MemorySegment dims = arena.allocateFrom(hdf5_h.C_LONG, 2, 3, 4);

            H5tid = hdf5_h_1.H5Tarray_create2(hdf5_h_1.H5T_NATIVE_DOUBLE_g(), 3, dims);
            assertTrue("H5Tarray_create2 failed", isValidId(H5tid));

            // Verify dimensions
            int ndims = hdf5_h_1.H5Tget_array_ndims(H5tid);
            assertEquals("Should be 3D array", 3, ndims);

            MemorySegment retrievedDims = arena.allocate(hdf5_h.C_LONG, 3);
            hdf5_h_1.H5Tget_array_dims2(H5tid, retrievedDims);
            assertEquals("First dimension should be 2", 2, retrievedDims.getAtIndex(hdf5_h.C_LONG, 0));
            assertEquals("Second dimension should be 3", 3, retrievedDims.getAtIndex(hdf5_h.C_LONG, 1));
            assertEquals("Third dimension should be 4", 4, retrievedDims.getAtIndex(hdf5_h.C_LONG, 2));
        }
    }

    @Test
    public void testH5Tget_array_ndims()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Test with different dimensionalities
            MemorySegment dims1 = arena.allocate(hdf5_h.C_LONG, 10);
            long tid1           = hdf5_h_1.H5Tarray_create2(hdf5_h_1.H5T_NATIVE_INT_g(), 1, dims1);
            assertEquals("Should be 1D", 1, hdf5_h_1.H5Tget_array_ndims(tid1));
            hdf5_h_1.H5Tclose(tid1);

            MemorySegment dims2 = arena.allocateFrom(hdf5_h.C_LONG, 5, 6);
            long tid2           = hdf5_h_1.H5Tarray_create2(hdf5_h_1.H5T_NATIVE_INT_g(), 2, dims2);
            assertEquals("Should be 2D", 2, hdf5_h_1.H5Tget_array_ndims(tid2));
            hdf5_h_1.H5Tclose(tid2);

            MemorySegment dims3 = arena.allocateFrom(hdf5_h.C_LONG, 2, 3, 4);
            H5tid               = hdf5_h_1.H5Tarray_create2(hdf5_h_1.H5T_NATIVE_INT_g(), 3, dims3);
            assertEquals("Should be 3D", 3, hdf5_h_1.H5Tget_array_ndims(H5tid));
        }
    }

    @Test
    public void testH5Tget_array_dims2()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create array with specific dimensions
            MemorySegment dims = arena.allocateFrom(hdf5_h.C_LONG, 7, 8, 9);
            H5tid              = hdf5_h_1.H5Tarray_create2(hdf5_h_1.H5T_NATIVE_LONG_g(), 3, dims);

            // Retrieve dimensions
            MemorySegment retrievedDims = arena.allocate(hdf5_h.C_LONG, 3);
            int result                  = hdf5_h_1.H5Tget_array_dims2(H5tid, retrievedDims);
            assertEquals("H5Tget_array_dims2 should return rank", 3, result);

            // Verify each dimension
            assertEquals("Dim 0 should be 7", 7, retrievedDims.getAtIndex(hdf5_h.C_LONG, 0));
            assertEquals("Dim 1 should be 8", 8, retrievedDims.getAtIndex(hdf5_h.C_LONG, 1));
            assertEquals("Dim 2 should be 9", 9, retrievedDims.getAtIndex(hdf5_h.C_LONG, 2));
        }
    }

    @Test
    public void testH5Tarray_with_compound_base()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create compound type
            long compoundType = hdf5_h_1.H5Tcreate(hdf5_h.H5T_COMPOUND(), 12);
            hdf5_h_1.H5Tinsert(compoundType, arena.allocateFrom("x"), 0, hdf5_h_1.H5T_NATIVE_INT_g());
            hdf5_h_1.H5Tinsert(compoundType, arena.allocateFrom("y"), 4, hdf5_h_1.H5T_NATIVE_INT_g());
            hdf5_h_1.H5Tinsert(compoundType, arena.allocateFrom("z"), 8, hdf5_h_1.H5T_NATIVE_INT_g());

            // Create array of compound types [5]
            MemorySegment dims = arena.allocate(hdf5_h.C_LONG, 5);
            H5tid              = hdf5_h_1.H5Tarray_create2(compoundType, 1, dims);
            assertTrue("H5Tarray_create2 with compound base failed", isValidId(H5tid));

            // Verify array properties
            assertEquals("Should be array type", hdf5_h.H5T_ARRAY(), hdf5_h_1.H5Tget_class(H5tid));
            assertEquals("Should be 1D", 1, hdf5_h_1.H5Tget_array_ndims(H5tid));

            // Get super type (base type)
            long superType = hdf5_h_1.H5Tget_super(H5tid);
            assertTrue("Should have valid super type", isValidId(superType));
            assertEquals("Super type should be compound", hdf5_h.H5T_COMPOUND(),
                         hdf5_h_1.H5Tget_class(superType));

            hdf5_h_1.H5Tclose(superType);
            hdf5_h_1.H5Tclose(compoundType);
        }
    }

    @Test
    public void testH5Tget_super_array()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create array type
            MemorySegment dims = arena.allocate(hdf5_h.C_LONG, 10);
            H5tid              = hdf5_h_1.H5Tarray_create2(hdf5_h_1.H5T_NATIVE_SHORT_g(), 1, dims);

            // Get the base type
            long superType = hdf5_h_1.H5Tget_super(H5tid);
            assertTrue("H5Tget_super failed", isValidId(superType));

            // Verify base type is short
            int equal = hdf5_h_1.H5Tequal(superType, hdf5_h_1.H5T_NATIVE_SHORT_g());
            assertTrue("Base type should be H5T_NATIVE_SHORT", equal > 0);

            hdf5_h_1.H5Tclose(superType);
        }
    }

    @Test
    public void testH5Tarray_size()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create array [5][10] of ints (4 bytes each)
            MemorySegment dims = arena.allocateFrom(hdf5_h.C_LONG, 5, 10);
            H5tid              = hdf5_h_1.H5Tarray_create2(hdf5_h_1.H5T_NATIVE_INT_g(), 2, dims);

            // Get size - should be 5 * 10 * 4 = 200 bytes
            long size = hdf5_h_1.H5Tget_size(H5tid);
            assertEquals("Array size should be 200 bytes", 200, size);

            // For doubles (8 bytes each): 5 * 10 * 8 = 400 bytes
            long tid2  = hdf5_h_1.H5Tarray_create2(hdf5_h_1.H5T_NATIVE_DOUBLE_g(), 2, dims);
            long size2 = hdf5_h_1.H5Tget_size(tid2);
            assertEquals("Array size should be 400 bytes", 400, size2);
            hdf5_h_1.H5Tclose(tid2);
        }
    }

    // ============================================================================
    // H5T Enum Datatype Tests
    // ============================================================================

    @Test
    public void testH5Tenum_create()
    {
        System.out.print(testname.getMethodName());

        H5tid = hdf5_h_1.H5Tenum_create(hdf5_h_1.H5T_NATIVE_INT_g());
        assertTrue("H5Tenum_create failed", isValidId(H5tid));

        // Verify it's an enum type
        int tclass = hdf5_h_1.H5Tget_class(H5tid);
        assertEquals("Should be enum type", hdf5_h.H5T_ENUM(), tclass);
    }

    @Test
    public void testH5Tenum_insert()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            H5tid = hdf5_h_1.H5Tenum_create(hdf5_h_1.H5T_NATIVE_INT_g());

            // Insert enum values
            MemorySegment val0 = arena.allocate(hdf5_h.C_INT, 0);
            int result         = hdf5_h_1.H5Tenum_insert(H5tid, arena.allocateFrom("RED"), val0);
            assertEquals("H5Tenum_insert RED failed", 0, result);

            MemorySegment val1 = arena.allocate(hdf5_h.C_INT, 1);
            result             = hdf5_h_1.H5Tenum_insert(H5tid, arena.allocateFrom("GREEN"), val1);
            assertEquals("H5Tenum_insert GREEN failed", 0, result);

            MemorySegment val2 = arena.allocate(hdf5_h.C_INT, 2);
            result             = hdf5_h_1.H5Tenum_insert(H5tid, arena.allocateFrom("BLUE"), val2);
            assertEquals("H5Tenum_insert BLUE failed", 0, result);

            // Verify member count
            int nmembers = hdf5_h_1.H5Tget_nmembers(H5tid);
            assertEquals("Should have 3 enum members", 3, nmembers);
        }
    }

    @Test
    public void testH5Tenum_insert_multiple()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            H5tid = hdf5_h_1.H5Tenum_create(hdf5_h_1.H5T_NATIVE_INT_g());

            // Insert multiple values
            String[] names = {"NORTH", "SOUTH", "EAST", "WEST"};
            for (int i = 0; i < names.length; i++) {
                MemorySegment val = arena.allocate(hdf5_h.C_INT, i * 10);
                hdf5_h_1.H5Tenum_insert(H5tid, arena.allocateFrom(names[i]), val);
            }

            assertEquals("Should have 4 members", 4, hdf5_h_1.H5Tget_nmembers(H5tid));
        }
    }

    @Test
    public void testH5Tenum_nameof()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            H5tid = hdf5_h_1.H5Tenum_create(hdf5_h_1.H5T_NATIVE_INT_g());

            // Insert enum values
            MemorySegment val0 = arena.allocate(hdf5_h.C_INT, 100);
            hdf5_h_1.H5Tenum_insert(H5tid, arena.allocateFrom("ALPHA"), val0);

            MemorySegment val1 = arena.allocate(hdf5_h.C_INT, 200);
            hdf5_h_1.H5Tenum_insert(H5tid, arena.allocateFrom("BETA"), val1);

            // Get name for value 100
            MemorySegment nameBuffer = arena.allocate(20);
            MemorySegment queryVal   = arena.allocate(hdf5_h.C_INT, 100);
            int result               = hdf5_h_1.H5Tenum_nameof(H5tid, queryVal, nameBuffer, 20);
            assertEquals("H5Tenum_nameof failed", 0, result);

            String name = nameBuffer.getString(0);
            assertEquals("Name should be ALPHA", "ALPHA", name);

            // Get name for value 200
            queryVal.set(hdf5_h.C_INT, 0, 200);
            result = hdf5_h_1.H5Tenum_nameof(H5tid, queryVal, nameBuffer, 20);
            assertEquals("H5Tenum_nameof failed", 0, result);

            name = nameBuffer.getString(0);
            assertEquals("Name should be BETA", "BETA", name);
        }
    }

    @Test
    public void testH5Tenum_valueof()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            H5tid = hdf5_h_1.H5Tenum_create(hdf5_h_1.H5T_NATIVE_INT_g());

            // Insert enum values
            MemorySegment val0 = arena.allocate(hdf5_h.C_INT, 42);
            hdf5_h_1.H5Tenum_insert(H5tid, arena.allocateFrom("MAGIC"), val0);

            MemorySegment val1 = arena.allocate(hdf5_h.C_INT, 99);
            hdf5_h_1.H5Tenum_insert(H5tid, arena.allocateFrom("SPECIAL"), val1);

            // Get value for name "MAGIC"
            MemorySegment retrievedVal = arena.allocate(hdf5_h.C_INT);
            int result = hdf5_h_1.H5Tenum_valueof(H5tid, arena.allocateFrom("MAGIC"), retrievedVal);
            assertEquals("H5Tenum_valueof failed", 0, result);
            assertEquals("Value should be 42", 42, retrievedVal.get(hdf5_h.C_INT, 0));

            // Get value for name "SPECIAL"
            result = hdf5_h_1.H5Tenum_valueof(H5tid, arena.allocateFrom("SPECIAL"), retrievedVal);
            assertEquals("H5Tenum_valueof failed", 0, result);
            assertEquals("Value should be 99", 99, retrievedVal.get(hdf5_h.C_INT, 0));
        }
    }

    @Test
    public void testH5Tenum_get_member_value()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            H5tid = hdf5_h_1.H5Tenum_create(hdf5_h_1.H5T_NATIVE_INT_g());

            // Insert values
            MemorySegment val0 = arena.allocate(hdf5_h.C_INT, 10);
            hdf5_h_1.H5Tenum_insert(H5tid, arena.allocateFrom("FIRST"), val0);

            MemorySegment val1 = arena.allocate(hdf5_h.C_INT, 20);
            hdf5_h_1.H5Tenum_insert(H5tid, arena.allocateFrom("SECOND"), val1);

            MemorySegment val2 = arena.allocate(hdf5_h.C_INT, 30);
            hdf5_h_1.H5Tenum_insert(H5tid, arena.allocateFrom("THIRD"), val2);

            // Get member values by index
            MemorySegment retrievedVal = arena.allocate(hdf5_h.C_INT);

            hdf5_h_1.H5Tget_member_value(H5tid, 0, retrievedVal);
            assertEquals("First member value should be 10", 10, retrievedVal.get(hdf5_h.C_INT, 0));

            hdf5_h_1.H5Tget_member_value(H5tid, 1, retrievedVal);
            assertEquals("Second member value should be 20", 20, retrievedVal.get(hdf5_h.C_INT, 0));

            hdf5_h_1.H5Tget_member_value(H5tid, 2, retrievedVal);
            assertEquals("Third member value should be 30", 30, retrievedVal.get(hdf5_h.C_INT, 0));
        }
    }

    @Test
    public void testH5Tenum_negative_values()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            H5tid = hdf5_h_1.H5Tenum_create(hdf5_h_1.H5T_NATIVE_INT_g());

            // Insert negative values (for error codes, etc.)
            MemorySegment valNeg1 = arena.allocate(hdf5_h.C_INT, -1);
            hdf5_h_1.H5Tenum_insert(H5tid, arena.allocateFrom("ERROR"), valNeg1);

            MemorySegment val0 = arena.allocate(hdf5_h.C_INT, 0);
            hdf5_h_1.H5Tenum_insert(H5tid, arena.allocateFrom("SUCCESS"), val0);

            MemorySegment val1 = arena.allocate(hdf5_h.C_INT, 1);
            hdf5_h_1.H5Tenum_insert(H5tid, arena.allocateFrom("WARNING"), val1);

            // Retrieve negative value
            MemorySegment retrievedVal = arena.allocate(hdf5_h.C_INT);
            hdf5_h_1.H5Tenum_valueof(H5tid, arena.allocateFrom("ERROR"), retrievedVal);
            assertEquals("Value should be -1", -1, retrievedVal.get(hdf5_h.C_INT, 0));
        }
    }

    @Test
    public void testH5Tget_nmembers_enum()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            H5tid = hdf5_h_1.H5Tenum_create(hdf5_h_1.H5T_NATIVE_INT_g());

            // Initially should have 0 members
            assertEquals("Empty enum should have 0 members", 0, hdf5_h_1.H5Tget_nmembers(H5tid));

            // Add members one by one and check count
            for (int i = 0; i < 5; i++) {
                MemorySegment val = arena.allocate(hdf5_h.C_INT, i);
                hdf5_h_1.H5Tenum_insert(H5tid, arena.allocateFrom("MEMBER_" + i), val);
                assertEquals("Should have " + (i + 1) + " members", i + 1, hdf5_h_1.H5Tget_nmembers(H5tid));
            }
        }
    }

    @Test
    public void testH5Tget_member_name_enum()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            H5tid = hdf5_h_1.H5Tenum_create(hdf5_h_1.H5T_NATIVE_INT_g());

            // Insert named values
            String[] expectedNames = {"SUNDAY", "MONDAY", "TUESDAY", "WEDNESDAY"};
            for (int i = 0; i < expectedNames.length; i++) {
                MemorySegment val = arena.allocate(hdf5_h.C_INT, i);
                hdf5_h_1.H5Tenum_insert(H5tid, arena.allocateFrom(expectedNames[i]), val);
            }

            // Retrieve names by index
            for (int i = 0; i < expectedNames.length; i++) {
                MemorySegment namePtr = hdf5_h_1.H5Tget_member_name(H5tid, i);
                assertNotNull("Name pointer should not be null", namePtr);

                String name = namePtr.getString(0);
                assertEquals("Member " + i + " name should match", expectedNames[i], name);

                // Free the allocated string
                hdf5_h_1.H5free_memory(namePtr);
            }
        }
    }

    // ============================================================================
    // H5T String Datatype Tests
    // ============================================================================

    @Test
    public void testH5Tcreate_string_variable()
    {
        System.out.print(testname.getMethodName());

        H5tid = hdf5_h_1.H5Tcopy(hdf5_h_1.H5T_C_S1_g());
        assertTrue("H5Tcopy for string failed", isValidId(H5tid));

        // Set to variable length
        int result = hdf5_h_1.H5Tset_size(H5tid, hdf5_h.H5T_VARIABLE());
        assertEquals("H5Tset_size to variable failed", 0, result);

        // Verify it's variable length
        long size = hdf5_h_1.H5Tget_size(H5tid);
        assertEquals("Should be variable length", hdf5_h.H5T_VARIABLE(), size);

        // Verify it's a string type
        assertEquals("Should be string class", hdf5_h.H5T_STRING(), hdf5_h_1.H5Tget_class(H5tid));
    }

    @Test
    public void testH5Tcreate_string_fixed()
    {
        System.out.print(testname.getMethodName());

        H5tid = hdf5_h_1.H5Tcopy(hdf5_h_1.H5T_C_S1_g());

        // Set to fixed length of 50 characters
        int result = hdf5_h_1.H5Tset_size(H5tid, 50);
        assertEquals("H5Tset_size failed", 0, result);

        // Verify size
        long size = hdf5_h_1.H5Tget_size(H5tid);
        assertEquals("String length should be 50", 50, size);
    }

    @Test
    public void testH5Tset_strpad()
    {
        System.out.print(testname.getMethodName());

        H5tid = hdf5_h_1.H5Tcopy(hdf5_h_1.H5T_C_S1_g());
        hdf5_h_1.H5Tset_size(H5tid, 20);

        // Test NULL padding
        int result = hdf5_h_1.H5Tset_strpad(H5tid, hdf5_h.H5T_STR_NULLPAD());
        assertEquals("H5Tset_strpad NULLPAD failed", 0, result);
        assertEquals("Should be NULLPAD", hdf5_h.H5T_STR_NULLPAD(), hdf5_h_1.H5Tget_strpad(H5tid));

        // Test NULL termination
        result = hdf5_h_1.H5Tset_strpad(H5tid, hdf5_h.H5T_STR_NULLTERM());
        assertEquals("H5Tset_strpad NULLTERM failed", 0, result);
        assertEquals("Should be NULLTERM", hdf5_h.H5T_STR_NULLTERM(), hdf5_h_1.H5Tget_strpad(H5tid));

        // Test SPACE padding
        result = hdf5_h_1.H5Tset_strpad(H5tid, hdf5_h.H5T_STR_SPACEPAD());
        assertEquals("H5Tset_strpad SPACEPAD failed", 0, result);
        assertEquals("Should be SPACEPAD", hdf5_h.H5T_STR_SPACEPAD(), hdf5_h_1.H5Tget_strpad(H5tid));
    }
}
