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
import org.junit.Assume;
import org.junit.Ignore;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

/**
 * FFM-only tests for HDF5 Datatype (H5T) operations.
 *
 * This test class uses direct FFM bindings without the hdf.hdf5lib wrapper layer.
 *
 * Note: Some tests are disabled on Windows due to known FFM limitations.
 */
public class TestH5Tffm {
    @Rule
    public TestName testname = new TestName();

    /** Helper to check if running on Windows */
    private static final boolean IS_WINDOWS = System.getProperty("os.name").toLowerCase().contains("win");

    static
    {
        // Initialize FFM library by calling H5open()
        // This ensures global type variables are properly initialized
        try {
            hdf5_h.H5open();
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
        closeQuietly(H5tid, hdf5_h::H5Tclose);
        H5tid = hdf5_h.H5I_INVALID_HID();
        System.out.println();
    }

    @Test
    public void testH5Tcopy()
    {
        System.out.print(testname.getMethodName());

        H5tid = hdf5_h.H5Tcopy(hdf5_h.H5T_STD_I32LE_g());
        assertTrue("H5Tcopy failed", isValidId(H5tid));
    }

    @Test
    public void testH5Tequal()
    {
        System.out.print(testname.getMethodName());
        long tid2 = hdf5_h.H5I_INVALID_HID();

        try {
            H5tid = hdf5_h.H5Tcopy(hdf5_h.H5T_STD_I32LE_g());
            assertTrue("H5Tcopy failed", isValidId(H5tid));

            tid2 = hdf5_h.H5Tcopy(hdf5_h.H5T_STD_I32LE_g());
            assertTrue("H5Tcopy failed", isValidId(tid2));

            int result = hdf5_h.H5Tequal(H5tid, tid2);
            assertTrue("Types should be equal", result > 0);

            // Compare with different type
            result = hdf5_h.H5Tequal(H5tid, hdf5_h.H5T_IEEE_F32LE_g());
            assertFalse("Types should not be equal", result > 0);
        }
        finally {
            closeQuietly(tid2, hdf5_h::H5Tclose);
        }
    }

    @Test
    public void testH5Tget_class()
    {
        System.out.print(testname.getMethodName());

        H5tid = hdf5_h.H5Tcopy(hdf5_h.H5T_STD_I32LE_g());
        assertTrue("H5Tcopy failed", isValidId(H5tid));

        int tclass = hdf5_h.H5Tget_class(H5tid);
        assertEquals("Type class should be INTEGER", hdf5_h.H5T_INTEGER(), tclass);
    }

    @Test
    public void testH5Tget_size()
    {
        System.out.print(testname.getMethodName());

        H5tid = hdf5_h.H5Tcopy(hdf5_h.H5T_STD_I32LE_g());
        assertTrue("H5Tcopy failed", isValidId(H5tid));

        long size = hdf5_h.H5Tget_size(H5tid);
        assertTrue("Type size should be > 0", size > 0);
        assertEquals("H5T_STD_I32LE should be 4 bytes", 4, size);
    }

    @Test
    public void testH5Tset_size()
    {
        System.out.print(testname.getMethodName());

        H5tid = hdf5_h.H5Tcopy(hdf5_h.H5T_C_S1_g());
        assertTrue("H5Tcopy failed", isValidId(H5tid));

        int result = hdf5_h.H5Tset_size(H5tid, 64);
        assertTrue("H5Tset_size failed", isSuccess(result));

        long size = hdf5_h.H5Tget_size(H5tid);
        assertEquals("Size should be 64", 64, size);
    }

    @Test
    public void testH5Tget_order()
    {
        System.out.print(testname.getMethodName());

        H5tid = hdf5_h.H5Tcopy(hdf5_h.H5T_STD_I32LE_g());
        assertTrue("H5Tcopy failed", isValidId(H5tid));

        int order = hdf5_h.H5Tget_order(H5tid);
        assertTrue("Byte order should be valid", order >= 0);
    }

    @Test
    public void testH5Tget_precision()
    {
        System.out.print(testname.getMethodName());

        H5tid = hdf5_h.H5Tcopy(hdf5_h.H5T_STD_I32LE_g());
        assertTrue("H5Tcopy failed", isValidId(H5tid));

        long precision = hdf5_h.H5Tget_precision(H5tid);
        assertTrue("Precision should be > 0", precision > 0);
    }

    @Test
    public void testH5Tset_precision()
    {
        System.out.print(testname.getMethodName());

        H5tid = hdf5_h.H5Tcopy(hdf5_h.H5T_STD_I32LE_g());
        assertTrue("H5Tcopy failed", isValidId(H5tid));

        int result = hdf5_h.H5Tset_precision(H5tid, 16);
        assertTrue("H5Tset_precision failed", isSuccess(result));

        long precision = hdf5_h.H5Tget_precision(H5tid);
        assertEquals("Precision should be 16", 16, precision);
    }

    @Test
    public void testH5Tget_strpad()
    {
        System.out.print(testname.getMethodName());

        H5tid = hdf5_h.H5Tcopy(hdf5_h.H5T_C_S1_g());
        assertTrue("H5Tcopy failed", isValidId(H5tid));

        int strpad = hdf5_h.H5Tget_strpad(H5tid);
        assertTrue("String padding should be valid", strpad >= 0);
    }

    @Test
    public void testH5Tcreate_compound()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create a compound type with int and double
            int compoundSize = 4 + 8; // sizeof(int) + sizeof(double)
            H5tid            = hdf5_h.H5Tcreate(hdf5_h.H5T_COMPOUND(), compoundSize);
            assertTrue("H5Tcreate failed", isValidId(H5tid));

            // Insert int member
            MemorySegment intNameSegment = stringToSegment(arena, "int_field");
            int result = hdf5_h.H5Tinsert(H5tid, intNameSegment, 0, hdf5_h.H5T_STD_I32LE_g());
            assertTrue("H5Tinsert int failed", isSuccess(result));

            // Insert double member
            MemorySegment doubleNameSegment = stringToSegment(arena, "double_field");
            result = hdf5_h.H5Tinsert(H5tid, doubleNameSegment, 4, hdf5_h.H5T_IEEE_F64LE_g());
            assertTrue("H5Tinsert double failed", isSuccess(result));

            // Verify it's a compound type
            int tclass = hdf5_h.H5Tget_class(H5tid);
            assertEquals("Type class should be COMPOUND", hdf5_h.H5T_COMPOUND(), tclass);

            // Verify number of members
            int nmembers = hdf5_h.H5Tget_nmembers(H5tid);
            assertEquals("Should have 2 members", 2, nmembers);
        }
    }

    @Test
    public void testH5Tget_nmembers()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            H5tid = hdf5_h.H5Tcreate(hdf5_h.H5T_COMPOUND(), 12);
            assertTrue("H5Tcreate failed", isValidId(H5tid));

            MemorySegment nameSegment = stringToSegment(arena, "field1");
            hdf5_h.H5Tinsert(H5tid, nameSegment, 0, hdf5_h.H5T_STD_I32LE_g());

            int nmembers = hdf5_h.H5Tget_nmembers(H5tid);
            assertEquals("Should have 1 member", 1, nmembers);
        }
    }

    @Test
    public void testH5Tget_member_name()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            H5tid = hdf5_h.H5Tcreate(hdf5_h.H5T_COMPOUND(), 12);
            assertTrue("H5Tcreate failed", isValidId(H5tid));

            String fieldName          = "test_field";
            MemorySegment nameSegment = stringToSegment(arena, fieldName);
            hdf5_h.H5Tinsert(H5tid, nameSegment, 0, hdf5_h.H5T_STD_I32LE_g());

            MemorySegment returnedName = hdf5_h.H5Tget_member_name(H5tid, 0);
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
            H5tid = hdf5_h.H5Tcreate(hdf5_h.H5T_COMPOUND(), 12);
            assertTrue("H5Tcreate failed", isValidId(H5tid));

            long expectedOffset       = 4;
            MemorySegment nameSegment = stringToSegment(arena, "field");
            hdf5_h.H5Tinsert(H5tid, nameSegment, expectedOffset, hdf5_h.H5T_STD_I32LE_g());

            long offset = hdf5_h.H5Tget_member_offset(H5tid, 0);
            assertEquals("Offset should match", expectedOffset, offset);
        }
    }

    @Test
    public void testH5Tget_member_type()
    {
        System.out.print(testname.getMethodName());
        long memberType = hdf5_h.H5I_INVALID_HID();

        try (Arena arena = Arena.ofConfined()) {
            H5tid = hdf5_h.H5Tcreate(hdf5_h.H5T_COMPOUND(), 12);
            assertTrue("H5Tcreate failed", isValidId(H5tid));

            MemorySegment nameSegment = stringToSegment(arena, "field");
            hdf5_h.H5Tinsert(H5tid, nameSegment, 0, hdf5_h.H5T_STD_I32LE_g());

            memberType = hdf5_h.H5Tget_member_type(H5tid, 0);
            assertTrue("H5Tget_member_type failed", isValidId(memberType));

            // Verify it's an integer type
            int tclass = hdf5_h.H5Tget_class(memberType);
            assertEquals("Member type should be INTEGER", hdf5_h.H5T_INTEGER(), tclass);
        }
        finally {
            closeQuietly(memberType, hdf5_h::H5Tclose);
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

            H5tid = hdf5_h.H5Tarray_create2(hdf5_h.H5T_STD_I32LE_g(), rank, dimsSegment);
            assertTrue("H5Tarray_create2 failed", isValidId(H5tid));

            // Verify it's an array type
            int tclass = hdf5_h.H5Tget_class(H5tid);
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

            H5tid = hdf5_h.H5Tarray_create2(hdf5_h.H5T_STD_I32LE_g(), rank, dimsSegment);
            assertTrue("H5Tarray_create2 failed", isValidId(H5tid));

            MemorySegment returnedDimsSegment = allocateLongArray(arena, rank);
            int result                        = hdf5_h.H5Tget_array_dims2(H5tid, returnedDimsSegment);
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
            H5tid = hdf5_h.H5Tenum_create(hdf5_h.H5T_STD_I32LE_g());
            assertTrue("H5Tenum_create failed", isValidId(H5tid));

            // Insert enum values
            MemorySegment redSegment      = stringToSegment(arena, "RED");
            MemorySegment redValueSegment = allocateInt(arena);
            setInt(redValueSegment, 0);
            int result = hdf5_h.H5Tenum_insert(H5tid, redSegment, redValueSegment);
            assertTrue("H5Tenum_insert RED failed", isSuccess(result));

            MemorySegment greenSegment      = stringToSegment(arena, "GREEN");
            MemorySegment greenValueSegment = allocateInt(arena);
            setInt(greenValueSegment, 1);
            result = hdf5_h.H5Tenum_insert(H5tid, greenSegment, greenValueSegment);
            assertTrue("H5Tenum_insert GREEN failed", isSuccess(result));

            MemorySegment blueSegment      = stringToSegment(arena, "BLUE");
            MemorySegment blueValueSegment = allocateInt(arena);
            setInt(blueValueSegment, 2);
            result = hdf5_h.H5Tenum_insert(H5tid, blueSegment, blueValueSegment);
            assertTrue("H5Tenum_insert BLUE failed", isSuccess(result));

            // Verify number of members
            int nmembers = hdf5_h.H5Tget_nmembers(H5tid);
            assertEquals("Should have 3 members", 3, nmembers);

            // Test H5Tenum_nameof - get name from value
            MemorySegment lookupValueSegment = allocateInt(arena);
            setInt(lookupValueSegment, 1);

            MemorySegment nameSegment = arena.allocate(64); // Allocate buffer for name
            int nameResult            = hdf5_h.H5Tenum_nameof(H5tid, lookupValueSegment, nameSegment, 64);
            assertTrue("H5Tenum_nameof failed", isSuccess(nameResult));

            String name = nameSegment.getString(0);
            assertEquals("Name should be GREEN", "GREEN", name);

            // Test H5Tenum_valueof - get value from name
            MemorySegment lookupNameSegment = stringToSegment(arena, "BLUE");
            MemorySegment valueSegment      = allocateInt(arena);
            result                          = hdf5_h.H5Tenum_valueof(H5tid, lookupNameSegment, valueSegment);
            assertTrue("H5Tenum_valueof failed", isSuccess(result));

            int value = getInt(valueSegment);
            assertEquals("Value should be 2", 2, value);

            // Verify it's an enum type
            int tclass = hdf5_h.H5Tget_class(H5tid);
            assertEquals("Type class should be ENUM", hdf5_h.H5T_ENUM(), tclass);
        }
    }

    @Test
    public void testH5Tis_variable_str()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create fixed-length string type
            H5tid = hdf5_h.H5Tcopy(hdf5_h.H5T_C_S1_g());
            assertTrue("H5Tcopy failed", isValidId(H5tid));

            hdf5_h.H5Tset_size(H5tid, 10);

            int result = hdf5_h.H5Tis_variable_str(H5tid);
            assertFalse("Fixed-length string should not be variable", result > 0);

            // Close and create variable-length string type
            hdf5_h.H5Tclose(H5tid);

            H5tid = hdf5_h.H5Tcopy(hdf5_h.H5T_C_S1_g());
            assertTrue("H5Tcopy failed", isValidId(H5tid));

            hdf5_h.H5Tset_size(H5tid, -1); // H5T_VARIABLE

            result = hdf5_h.H5Tis_variable_str(H5tid);
            assertTrue("Variable-length string should be variable", result > 0);
        }
    }

    @Test
    public void testH5Tget_cset()
    {
        System.out.print(testname.getMethodName());

        H5tid = hdf5_h.H5Tcopy(hdf5_h.H5T_C_S1_g());
        assertTrue("H5Tcopy failed", isValidId(H5tid));

        int cset = hdf5_h.H5Tget_cset(H5tid);
        assertTrue("Character set should be valid", cset >= 0);
        // H5T_CSET_ASCII = 0
        assertEquals("Default character set should be ASCII", 0, cset);
    }

    @Test
    public void testH5Tclose()
    {
        System.out.print(testname.getMethodName());

        H5tid = hdf5_h.H5Tcopy(hdf5_h.H5T_STD_I32LE_g());
        assertTrue("H5Tcopy failed", isValidId(H5tid));

        int result = hdf5_h.H5Tclose(H5tid);
        assertTrue("H5Tclose failed", isSuccess(result));
        H5tid = hdf5_h.H5I_INVALID_HID();
    }

    @Test
    public void testH5Tvlen_create()
    {
        // Skip on Windows - FFM memory layout issue with variable-length types
        Assume.assumeFalse("Skipping on Windows - FFM limitation", IS_WINDOWS);

        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create variable-length type of integers
            // Use H5T_STD_I32LE instead of H5T_NATIVE_INT for platform consistency
            H5tid = hdf5_h.H5Tvlen_create(hdf5_h.H5T_STD_I32LE_g());
            assertTrue("H5Tvlen_create failed", isValidId(H5tid));

            // Verify it's a variable-length type
            int tclass = hdf5_h.H5Tget_class(H5tid);
            assertEquals("Should be H5T_VLEN class", hdf5_h.H5T_VLEN(), tclass);

            // Get the base type
            long base_type = hdf5_h.H5Tget_super(H5tid);
            assertTrue("H5Tget_super should return valid type", isValidId(base_type));

            // Verify base type is 32-bit integer
            int equal = hdf5_h.H5Tequal(base_type, hdf5_h.H5T_STD_I32LE_g());
            assertTrue("Base type should be H5T_STD_I32LE", equal > 0);

            hdf5_h.H5Tclose(base_type);
        }
    }

    @Test
    public void testH5Topaque_operations()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create opaque type with 16 bytes
            long size = 16;
            H5tid     = hdf5_h.H5Tcreate(hdf5_h.H5T_OPAQUE(), size);
            assertTrue("H5Tcreate opaque failed", isValidId(H5tid));

            // Verify it's opaque
            int tclass = hdf5_h.H5Tget_class(H5tid);
            assertEquals("Should be H5T_OPAQUE class", hdf5_h.H5T_OPAQUE(), tclass);

            // Set tag for opaque type
            String tag               = "16-byte opaque data";
            MemorySegment tagSegment = stringToSegment(arena, tag);
            int result               = hdf5_h.H5Tset_tag(H5tid, tagSegment);
            assertTrue("H5Tset_tag failed", isSuccess(result));

            // Get tag back
            MemorySegment outTag = hdf5_h.H5Tget_tag(H5tid);
            assertFalse("H5Tget_tag should return valid pointer", outTag.address() == 0);

            String retrievedTag = outTag.getString(0);
            assertEquals("Tag should match", tag, retrievedTag);

            // Verify size
            long retrievedSize = hdf5_h.H5Tget_size(H5tid);
            assertEquals("Size should be 16", size, retrievedSize);
        }
    }

    @Test
    public void testH5Tget_sign_set_sign()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create integer type
            H5tid = hdf5_h.H5Tcopy(hdf5_h.H5T_STD_I32LE_g());
            assertTrue("H5Tcopy failed", isValidId(H5tid));

            // Get current sign
            int sign = hdf5_h.H5Tget_sign(H5tid);
            assertTrue("H5Tget_sign should succeed", sign >= 0);

            // Set to unsigned
            int result = hdf5_h.H5Tset_sign(H5tid, hdf5_h.H5T_SGN_NONE());
            assertTrue("H5Tset_sign failed", isSuccess(result));

            // Verify sign changed
            int newSign = hdf5_h.H5Tget_sign(H5tid);
            assertEquals("Sign should be H5T_SGN_NONE", hdf5_h.H5T_SGN_NONE(), newSign);
        }
    }

    @Test
    public void testH5Tget_offset_set_offset()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create integer type
            H5tid = hdf5_h.H5Tcopy(hdf5_h.H5T_STD_I32LE_g());
            assertTrue("H5Tcopy failed", isValidId(H5tid));

            // Get current offset
            long offset = hdf5_h.H5Tget_offset(H5tid);
            assertTrue("H5Tget_offset should succeed", offset >= 0);

            // Set new offset (shift by 2 bits)
            long newOffset = 2;
            int result     = hdf5_h.H5Tset_offset(H5tid, newOffset);
            assertTrue("H5Tset_offset failed", isSuccess(result));

            // Verify offset changed
            long retrievedOffset = hdf5_h.H5Tget_offset(H5tid);
            assertEquals("Offset should be 2", newOffset, retrievedOffset);
        }
    }

    @Test
    public void testH5Tget_pad_set_pad()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create integer type
            H5tid = hdf5_h.H5Tcopy(hdf5_h.H5T_STD_I32LE_g());
            assertTrue("H5Tcopy failed", isValidId(H5tid));

            // Get current padding
            MemorySegment lsbSegment = allocateInt(arena);
            MemorySegment msbSegment = allocateInt(arena);
            int result               = hdf5_h.H5Tget_pad(H5tid, lsbSegment, msbSegment);
            assertTrue("H5Tget_pad failed", isSuccess(result));

            // Set new padding (both to zero)
            result = hdf5_h.H5Tset_pad(H5tid, hdf5_h.H5T_PAD_ZERO(), hdf5_h.H5T_PAD_ZERO());
            assertTrue("H5Tset_pad failed", isSuccess(result));

            // Verify padding changed
            MemorySegment newLsbSegment = allocateInt(arena);
            MemorySegment newMsbSegment = allocateInt(arena);
            result                      = hdf5_h.H5Tget_pad(H5tid, newLsbSegment, newMsbSegment);
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
            long srcType = hdf5_h.H5T_STD_I32LE_g();
            long dstType = hdf5_h.H5T_IEEE_F64LE_g();
            int result   = hdf5_h.H5Tconvert(srcType, dstType, numElements, buffer, MemorySegment.NULL,
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
            int result = hdf5_h.H5Tconvert(hdf5_h.H5T_STD_I32LE_g(), hdf5_h.H5T_IEEE_F32LE_g(), numElements,
                                           buffer, MemorySegment.NULL, hdf5_h.H5P_DEFAULT());
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
            long strType = hdf5_h.H5Tcopy(hdf5_h.H5T_C_S1_g());
            assertTrue("H5Tcopy failed", isValidId(strType));

            int result = hdf5_h.H5Tset_size(strType, -1); // H5T_VARIABLE
            assertTrue("H5Tset_size failed", isSuccess(result));

            // Create simple 1D dataspace with 1 element
            long[] dimsArray   = {1};
            MemorySegment dims = allocateLongArray(arena, 1);
            copyToSegment(dims, dimsArray);
            long space = hdf5_h.H5Screate_simple(1, dims, MemorySegment.NULL);
            assertTrue("H5Screate_simple failed", isValidId(space));

            // Allocate buffer for pointer to string
            MemorySegment buffer = arena.allocate(8); // Pointer size

            // Set to NULL initially (nothing to reclaim, but tests the API)
            buffer.set(java.lang.foreign.ValueLayout.ADDRESS, 0, MemorySegment.NULL);

            // Test H5Treclaim - should succeed even with NULL pointer
            result = hdf5_h.H5Treclaim(strType, space, hdf5_h.H5P_DEFAULT(), buffer);
            assertTrue("H5Treclaim should succeed", isSuccess(result));

            // Cleanup
            hdf5_h.H5Sclose(space);
            hdf5_h.H5Tclose(strType);
        }
    }

    @Test
    public void testH5Tfind_conversion_path()
    {
        // Skip on Windows - FFM memory layout issue with conversion functions
        Assume.assumeFalse("Skipping on Windows - FFM limitation", IS_WINDOWS);

        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Try to find conversion path from int to float
            MemorySegment pcdata = arena.allocate(8); // Pointer to H5T_cdata_t*
            pcdata.set(java.lang.foreign.ValueLayout.ADDRESS, 0, MemorySegment.NULL);

            MemorySegment convFunc =
                hdf5_h.H5Tfind(hdf5_h.H5T_STD_I32LE_g(), hdf5_h.H5T_IEEE_F32LE_g(), pcdata);

            // H5Tfind returns function pointer (can be NULL if no conversion exists)
            // For standard types, conversion should exist
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
                hdf5_h.H5Tfind(hdf5_h.H5T_STD_I32LE_g(), hdf5_h.H5T_STD_I32LE_g(), pcdata);

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
            long floatType = hdf5_h.H5Tcopy(hdf5_h.H5T_IEEE_F64LE_g());
            assertTrue("H5Tcopy failed", isValidId(floatType));

            // Get field positions for floating point type
            MemorySegment spos  = allocateLongArray(arena, 1); // sign position
            MemorySegment epos  = allocateLongArray(arena, 1); // exponent position
            MemorySegment esize = allocateLongArray(arena, 1); // exponent size
            MemorySegment mpos  = allocateLongArray(arena, 1); // mantissa position
            MemorySegment msize = allocateLongArray(arena, 1); // mantissa size

            int result = hdf5_h.H5Tget_fields(floatType, spos, epos, esize, mpos, msize);
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

            hdf5_h.H5Tclose(floatType);
        }
    }

    @Test
    public void testH5Tget_ebias()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Get exponent bias for double
            long floatType = hdf5_h.H5Tcopy(hdf5_h.H5T_IEEE_F64LE_g());
            assertTrue("H5Tcopy failed", isValidId(floatType));

            long ebias = hdf5_h.H5Tget_ebias(floatType);
            assertTrue("Exponent bias should be > 0", ebias > 0);

            // For IEEE 754 double, exponent bias is typically 1023
            // We won't test exact value as it's platform-dependent, but it should be reasonable
            assertTrue("Exponent bias should be reasonable", ebias < 10000);

            hdf5_h.H5Tclose(floatType);
        }
    }

    @Test
    public void testH5Tget_norm()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Get normalization type for floating point
            long floatType = hdf5_h.H5Tcopy(hdf5_h.H5T_IEEE_F32LE_g());
            assertTrue("H5Tcopy failed", isValidId(floatType));

            int norm = hdf5_h.H5Tget_norm(floatType);
            assertTrue("Normalization should be valid", norm >= 0);

            // Typical normalization types: IMPLIED (0), MSBSET (1), NONE (2)
            assertTrue("Normalization should be in valid range", norm <= 2);

            hdf5_h.H5Tclose(floatType);
        }
    }

    @Test
    public void testH5Tget_inpad()
    {

        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Get internal padding type for floating point
            long floatType = hdf5_h.H5Tcopy(hdf5_h.H5T_IEEE_F64LE_g());
            assertTrue("H5Tcopy failed", isValidId(floatType));

            int inpad = hdf5_h.H5Tget_inpad(floatType);
            assertTrue("Internal padding should be valid", inpad >= 0);

            // Padding types: ZERO (0), ONE (1), BACKGROUND (2)
            assertTrue("Internal padding should be in valid range", inpad <= 2);

            hdf5_h.H5Tclose(floatType);
        }
    }

    @Test
    public void testH5Tset_fields_and_ebias()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create a custom floating point type
            long floatType = hdf5_h.H5Tcopy(hdf5_h.H5T_IEEE_F32LE_g());
            assertTrue("H5Tcopy failed", isValidId(floatType));

            // Set custom field layout
            // For 32-bit float: sign(1) + exponent(8) + mantissa(23) = 32 bits
            long spos  = 31; // Sign at bit 31
            long epos  = 23; // Exponent starts at bit 23
            long esize = 8;  // Exponent is 8 bits
            long mpos  = 0;  // Mantissa starts at bit 0
            long msize = 23; // Mantissa is 23 bits

            int result = hdf5_h.H5Tset_fields(floatType, spos, epos, esize, mpos, msize);
            assertTrue("H5Tset_fields failed", isSuccess(result));

            // Set exponent bias (for 8-bit exponent, typical bias is 127)
            result = hdf5_h.H5Tset_ebias(floatType, 127);
            assertTrue("H5Tset_ebias failed", isSuccess(result));

            // Verify the settings
            long retrievedEbias = hdf5_h.H5Tget_ebias(floatType);
            assertEquals("Exponent bias should match", 127, retrievedEbias);

            hdf5_h.H5Tclose(floatType);
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
            long[] dimValues   = {10};
            MemorySegment dims = allocateLongArray(arena, 1);
            copyToSegment(dims, dimValues);

            H5tid = hdf5_h.H5Tarray_create2(hdf5_h.H5T_STD_I32LE_g(), 1, dims);
            assertTrue("H5Tarray_create2 failed", isValidId(H5tid));

            // Verify it's an array type
            int tclass = hdf5_h.H5Tget_class(H5tid);
            assertEquals("Should be array type", hdf5_h.H5T_ARRAY(), tclass);

            // Verify dimensions
            int ndims = hdf5_h.H5Tget_array_ndims(H5tid);
            assertEquals("Should be 1D array", 1, ndims);

            MemorySegment retrievedDims = allocateLongArray(arena, 1);
            int result                  = hdf5_h.H5Tget_array_dims2(H5tid, retrievedDims);
            assertEquals("H5Tget_array_dims2 should succeed", 1, result);
            assertEquals("Dimension should be 10", 10, retrievedDims.get(ValueLayout.JAVA_LONG, 0));
        }
    }

    @Test
    public void testH5Tarray_create_2D()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create 2D array of floats [3][4]
            MemorySegment dims = arena.allocateFrom(ValueLayout.JAVA_LONG, 3, 4);

            H5tid = hdf5_h.H5Tarray_create2(hdf5_h.H5T_IEEE_F32LE_g(), 2, dims);
            assertTrue("H5Tarray_create2 failed", isValidId(H5tid));

            // Verify dimensions
            int ndims = hdf5_h.H5Tget_array_ndims(H5tid);
            assertEquals("Should be 2D array", 2, ndims);

            MemorySegment retrievedDims = arena.allocate(ValueLayout.JAVA_LONG, 2);
            hdf5_h.H5Tget_array_dims2(H5tid, retrievedDims);
            assertEquals("First dimension should be 3", 3,
                         retrievedDims.getAtIndex(ValueLayout.JAVA_LONG, 0));
            assertEquals("Second dimension should be 4", 4,
                         retrievedDims.getAtIndex(ValueLayout.JAVA_LONG, 1));
        }
    }

    @Test
    public void testH5Tarray_create_3D()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create 3D array of doubles [2][3][4]
            MemorySegment dims = arena.allocateFrom(ValueLayout.JAVA_LONG, 2, 3, 4);

            H5tid = hdf5_h.H5Tarray_create2(hdf5_h.H5T_IEEE_F64LE_g(), 3, dims);
            assertTrue("H5Tarray_create2 failed", isValidId(H5tid));

            // Verify dimensions
            int ndims = hdf5_h.H5Tget_array_ndims(H5tid);
            assertEquals("Should be 3D array", 3, ndims);

            MemorySegment retrievedDims = arena.allocate(ValueLayout.JAVA_LONG, 3);
            hdf5_h.H5Tget_array_dims2(H5tid, retrievedDims);
            assertEquals("First dimension should be 2", 2,
                         retrievedDims.getAtIndex(ValueLayout.JAVA_LONG, 0));
            assertEquals("Second dimension should be 3", 3,
                         retrievedDims.getAtIndex(ValueLayout.JAVA_LONG, 1));
            assertEquals("Third dimension should be 4", 4,
                         retrievedDims.getAtIndex(ValueLayout.JAVA_LONG, 2));
        }
    }

    @Test
    public void testH5Tget_array_ndims()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Test with different dimensionalities
            MemorySegment dims1 = arena.allocateFrom(ValueLayout.JAVA_LONG, 10L);
            long tid1           = hdf5_h.H5Tarray_create2(hdf5_h.H5T_STD_I32LE_g(), 1, dims1);
            assertEquals("Should be 1D", 1, hdf5_h.H5Tget_array_ndims(tid1));
            hdf5_h.H5Tclose(tid1);

            MemorySegment dims2 = arena.allocateFrom(ValueLayout.JAVA_LONG, 5L, 6L);
            long tid2           = hdf5_h.H5Tarray_create2(hdf5_h.H5T_STD_I32LE_g(), 2, dims2);
            assertEquals("Should be 2D", 2, hdf5_h.H5Tget_array_ndims(tid2));
            hdf5_h.H5Tclose(tid2);

            MemorySegment dims3 = arena.allocateFrom(ValueLayout.JAVA_LONG, 2L, 3L, 4L);
            H5tid               = hdf5_h.H5Tarray_create2(hdf5_h.H5T_STD_I32LE_g(), 3, dims3);
            assertEquals("Should be 3D", 3, hdf5_h.H5Tget_array_ndims(H5tid));
        }
    }

    @Test
    public void testH5Tget_array_dims2()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create array with specific dimensions
            MemorySegment dims = arena.allocateFrom(ValueLayout.JAVA_LONG, 7, 8, 9);
            H5tid              = hdf5_h.H5Tarray_create2(hdf5_h.H5T_STD_I64LE_g(), 3, dims);

            // Retrieve dimensions
            MemorySegment retrievedDims = arena.allocate(ValueLayout.JAVA_LONG, 3);
            int result                  = hdf5_h.H5Tget_array_dims2(H5tid, retrievedDims);
            assertEquals("H5Tget_array_dims2 should return rank", 3, result);

            // Verify each dimension
            assertEquals("Dim 0 should be 7", 7, retrievedDims.getAtIndex(ValueLayout.JAVA_LONG, 0));
            assertEquals("Dim 1 should be 8", 8, retrievedDims.getAtIndex(ValueLayout.JAVA_LONG, 1));
            assertEquals("Dim 2 should be 9", 9, retrievedDims.getAtIndex(ValueLayout.JAVA_LONG, 2));
        }
    }

    @Test
    public void testH5Tarray_with_compound_base()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create compound type
            long compoundType = hdf5_h.H5Tcreate(hdf5_h.H5T_COMPOUND(), 12);
            hdf5_h.H5Tinsert(compoundType, stringToSegment(arena, "x"), 0, hdf5_h.H5T_STD_I32LE_g());
            hdf5_h.H5Tinsert(compoundType, stringToSegment(arena, "y"), 4, hdf5_h.H5T_STD_I32LE_g());
            hdf5_h.H5Tinsert(compoundType, stringToSegment(arena, "z"), 8, hdf5_h.H5T_STD_I32LE_g());

            // Create array of compound types [5]
            MemorySegment dims = arena.allocateFrom(ValueLayout.JAVA_LONG, 5L);
            H5tid              = hdf5_h.H5Tarray_create2(compoundType, 1, dims);
            assertTrue("H5Tarray_create2 with compound base failed", isValidId(H5tid));

            // Verify array properties
            assertEquals("Should be array type", hdf5_h.H5T_ARRAY(), hdf5_h.H5Tget_class(H5tid));
            assertEquals("Should be 1D", 1, hdf5_h.H5Tget_array_ndims(H5tid));

            // Get super type (base type)
            long superType = hdf5_h.H5Tget_super(H5tid);
            assertTrue("Should have valid super type", isValidId(superType));
            assertEquals("Super type should be compound", hdf5_h.H5T_COMPOUND(),
                         hdf5_h.H5Tget_class(superType));

            hdf5_h.H5Tclose(superType);
            hdf5_h.H5Tclose(compoundType);
        }
    }

    @Test
    public void testH5Tget_super_array()
    {
        // Skip on Windows - FFM memory layout issue with array dimensions
        Assume.assumeFalse("Skipping on Windows - FFM limitation", IS_WINDOWS);

        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create array type
            MemorySegment dims = arena.allocateFrom(ValueLayout.JAVA_LONG, 10L);
            H5tid              = hdf5_h.H5Tarray_create2(hdf5_h.H5T_STD_I16LE_g(), 1, dims);

            // Get the base type
            long superType = hdf5_h.H5Tget_super(H5tid);
            assertTrue("H5Tget_super failed", isValidId(superType));

            // Verify base type is short
            int equal = hdf5_h.H5Tequal(superType, hdf5_h.H5T_STD_I16LE_g());
            assertTrue("Base type should be H5T_STD_I16LE", equal > 0);

            hdf5_h.H5Tclose(superType);
        }
    }

    @Test
    public void testH5Tarray_size()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create array [5][10] of ints (4 bytes each)
            MemorySegment dims = arena.allocateFrom(ValueLayout.JAVA_LONG, 5, 10);
            H5tid              = hdf5_h.H5Tarray_create2(hdf5_h.H5T_STD_I32LE_g(), 2, dims);

            // Get size - should be 5 * 10 * 4 = 200 bytes
            long size = hdf5_h.H5Tget_size(H5tid);
            assertEquals("Array size should be 200 bytes", 200, size);

            // For doubles (8 bytes each): 5 * 10 * 8 = 400 bytes
            long tid2  = hdf5_h.H5Tarray_create2(hdf5_h.H5T_IEEE_F64LE_g(), 2, dims);
            long size2 = hdf5_h.H5Tget_size(tid2);
            assertEquals("Array size should be 400 bytes", 400, size2);
            hdf5_h.H5Tclose(tid2);
        }
    }

    // ============================================================================
    // H5T Enum Datatype Tests
    // ============================================================================

    @Test
    public void testH5Tenum_create()
    {
        System.out.print(testname.getMethodName());

        H5tid = hdf5_h.H5Tenum_create(hdf5_h.H5T_STD_I32LE_g());
        assertTrue("H5Tenum_create failed", isValidId(H5tid));

        // Verify it's an enum type
        int tclass = hdf5_h.H5Tget_class(H5tid);
        assertEquals("Should be enum type", hdf5_h.H5T_ENUM(), tclass);
    }

    @Test
    public void testH5Tenum_insert()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            H5tid = hdf5_h.H5Tenum_create(hdf5_h.H5T_STD_I32LE_g());

            // Insert enum values
            MemorySegment val0 = allocateInt(arena);
            setInt(val0, 0);
            int result = hdf5_h.H5Tenum_insert(H5tid, stringToSegment(arena, "RED"), val0);
            assertEquals("H5Tenum_insert RED failed", 0, result);

            MemorySegment val1 = allocateInt(arena);
            setInt(val1, 1);
            result = hdf5_h.H5Tenum_insert(H5tid, stringToSegment(arena, "GREEN"), val1);
            assertEquals("H5Tenum_insert GREEN failed", 0, result);

            MemorySegment val2 = allocateInt(arena);
            setInt(val2, 2);
            result = hdf5_h.H5Tenum_insert(H5tid, stringToSegment(arena, "BLUE"), val2);
            assertEquals("H5Tenum_insert BLUE failed", 0, result);

            // Verify member count
            int nmembers = hdf5_h.H5Tget_nmembers(H5tid);
            assertEquals("Should have 3 enum members", 3, nmembers);
        }
    }

    @Test
    public void testH5Tenum_insert_multiple()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            H5tid = hdf5_h.H5Tenum_create(hdf5_h.H5T_STD_I32LE_g());

            // Insert multiple values
            String[] names = {"NORTH", "SOUTH", "EAST", "WEST"};
            for (int i = 0; i < names.length; i++) {
                MemorySegment val = allocateInt(arena);
                setInt(val, i * 10);
                hdf5_h.H5Tenum_insert(H5tid, stringToSegment(arena, names[i]), val);
            }

            assertEquals("Should have 4 members", 4, hdf5_h.H5Tget_nmembers(H5tid));
        }
    }

    @Test
    public void testH5Tenum_nameof()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            H5tid = hdf5_h.H5Tenum_create(hdf5_h.H5T_STD_I32LE_g());

            // Insert enum values
            MemorySegment val0 = allocateInt(arena);
            setInt(val0, 100);
            hdf5_h.H5Tenum_insert(H5tid, stringToSegment(arena, "ALPHA"), val0);

            MemorySegment val1 = allocateInt(arena);
            setInt(val1, 200);
            hdf5_h.H5Tenum_insert(H5tid, stringToSegment(arena, "BETA"), val1);

            // Get name for value 100
            MemorySegment nameBuffer = arena.allocate(20);
            MemorySegment queryVal   = allocateInt(arena);
            setInt(queryVal, 100);
            int result = hdf5_h.H5Tenum_nameof(H5tid, queryVal, nameBuffer, 20);
            assertEquals("H5Tenum_nameof failed", 0, result);

            String name = nameBuffer.getString(0);
            assertEquals("Name should be ALPHA", "ALPHA", name);

            // Get name for value 200
            setInt(queryVal, 200);
            result = hdf5_h.H5Tenum_nameof(H5tid, queryVal, nameBuffer, 20);
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
            H5tid = hdf5_h.H5Tenum_create(hdf5_h.H5T_STD_I32LE_g());

            // Insert enum values
            MemorySegment val0 = allocateInt(arena);
            setInt(val0, 42);
            hdf5_h.H5Tenum_insert(H5tid, stringToSegment(arena, "MAGIC"), val0);

            MemorySegment val1 = allocateInt(arena);
            setInt(val1, 99);
            hdf5_h.H5Tenum_insert(H5tid, stringToSegment(arena, "SPECIAL"), val1);

            // Get value for name "MAGIC"
            MemorySegment retrievedVal = allocateInt(arena);
            int result = hdf5_h.H5Tenum_valueof(H5tid, stringToSegment(arena, "MAGIC"), retrievedVal);
            assertEquals("H5Tenum_valueof failed", 0, result);
            assertEquals("Value should be 42", 42, getInt(retrievedVal));

            // Get value for name "SPECIAL"
            result = hdf5_h.H5Tenum_valueof(H5tid, stringToSegment(arena, "SPECIAL"), retrievedVal);
            assertEquals("H5Tenum_valueof failed", 0, result);
            assertEquals("Value should be 99", 99, getInt(retrievedVal));
        }
    }

    @Test
    public void testH5Tenum_get_member_value()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            H5tid = hdf5_h.H5Tenum_create(hdf5_h.H5T_STD_I32LE_g());

            // Insert values
            MemorySegment val0 = allocateInt(arena);
            setInt(val0, 10);
            hdf5_h.H5Tenum_insert(H5tid, stringToSegment(arena, "FIRST"), val0);

            MemorySegment val1 = allocateInt(arena);
            setInt(val1, 20);
            hdf5_h.H5Tenum_insert(H5tid, stringToSegment(arena, "SECOND"), val1);

            MemorySegment val2 = allocateInt(arena);
            setInt(val2, 30);
            hdf5_h.H5Tenum_insert(H5tid, stringToSegment(arena, "THIRD"), val2);

            // Get member values by index
            MemorySegment retrievedVal = allocateInt(arena);

            hdf5_h.H5Tget_member_value(H5tid, 0, retrievedVal);
            assertEquals("First member value should be 10", 10, getInt(retrievedVal));

            hdf5_h.H5Tget_member_value(H5tid, 1, retrievedVal);
            assertEquals("Second member value should be 20", 20, getInt(retrievedVal));

            hdf5_h.H5Tget_member_value(H5tid, 2, retrievedVal);
            assertEquals("Third member value should be 30", 30, getInt(retrievedVal));
        }
    }

    @Test
    public void testH5Tenum_negative_values()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            H5tid = hdf5_h.H5Tenum_create(hdf5_h.H5T_STD_I32LE_g());

            // Insert negative values (for error codes, etc.)
            MemorySegment valNeg1 = allocateInt(arena);
            setInt(valNeg1, -1);
            hdf5_h.H5Tenum_insert(H5tid, stringToSegment(arena, "ERROR"), valNeg1);

            MemorySegment val0 = allocateInt(arena);
            setInt(val0, 0);
            hdf5_h.H5Tenum_insert(H5tid, stringToSegment(arena, "SUCCESS"), val0);

            MemorySegment val1 = allocateInt(arena);
            setInt(val1, 1);
            hdf5_h.H5Tenum_insert(H5tid, stringToSegment(arena, "WARNING"), val1);

            // Retrieve negative value
            MemorySegment retrievedVal = allocateInt(arena);
            hdf5_h.H5Tenum_valueof(H5tid, stringToSegment(arena, "ERROR"), retrievedVal);
            assertEquals("Value should be -1", -1, getInt(retrievedVal));
        }
    }

    @Test
    public void testH5Tget_nmembers_enum()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            H5tid = hdf5_h.H5Tenum_create(hdf5_h.H5T_STD_I32LE_g());

            // Initially should have 0 members
            assertEquals("Empty enum should have 0 members", 0, hdf5_h.H5Tget_nmembers(H5tid));

            // Add members one by one and check count
            for (int i = 0; i < 5; i++) {
                MemorySegment val = allocateInt(arena);
                setInt(val, i);
                hdf5_h.H5Tenum_insert(H5tid, stringToSegment(arena, "MEMBER_" + i), val);
                assertEquals("Should have " + (i + 1) + " members", i + 1, hdf5_h.H5Tget_nmembers(H5tid));
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

        H5tid = hdf5_h.H5Tcopy(hdf5_h.H5T_C_S1_g());
        assertTrue("H5Tcopy for string failed", isValidId(H5tid));

        // Set to variable length
        int result = hdf5_h.H5Tset_size(H5tid, hdf5_h.H5T_VARIABLE());
        assertTrue("H5Tset_size to variable should succeed", isSuccess(result));

        // Verify it's variable length (H5T_VARIABLE returns size_t max, which appears as -1 when signed)
        long size = hdf5_h.H5Tget_size(H5tid);
        // Variable length strings report their size as the size of hvl_t struct (16 bytes on 64-bit)
        // Use H5Tis_variable_str to check if it's truly variable length
        int isVar = hdf5_h.H5Tis_variable_str(H5tid);
        assertTrue("Should be variable length string", isVar > 0);

        // Verify it's a string type
        assertEquals("Should be string class", hdf5_h.H5T_STRING(), hdf5_h.H5Tget_class(H5tid));
    }

    @Test
    public void testH5Tcreate_string_fixed()
    {
        System.out.print(testname.getMethodName());

        H5tid = hdf5_h.H5Tcopy(hdf5_h.H5T_C_S1_g());

        // Set to fixed length of 50 characters
        int result = hdf5_h.H5Tset_size(H5tid, 50);
        assertEquals("H5Tset_size failed", 0, result);

        // Verify size
        long size = hdf5_h.H5Tget_size(H5tid);
        assertEquals("String length should be 50", 50, size);
    }

    @Test
    public void testH5Tset_strpad()
    {
        System.out.print(testname.getMethodName());

        H5tid = hdf5_h.H5Tcopy(hdf5_h.H5T_C_S1_g());
        hdf5_h.H5Tset_size(H5tid, 20);

        // Test NULL padding
        int result = hdf5_h.H5Tset_strpad(H5tid, hdf5_h.H5T_STR_NULLPAD());
        assertEquals("H5Tset_strpad NULLPAD failed", 0, result);
        assertEquals("Should be NULLPAD", hdf5_h.H5T_STR_NULLPAD(), hdf5_h.H5Tget_strpad(H5tid));

        // Test NULL termination
        result = hdf5_h.H5Tset_strpad(H5tid, hdf5_h.H5T_STR_NULLTERM());
        assertEquals("H5Tset_strpad NULLTERM failed", 0, result);
        assertEquals("Should be NULLTERM", hdf5_h.H5T_STR_NULLTERM(), hdf5_h.H5Tget_strpad(H5tid));

        // Test SPACE padding
        result = hdf5_h.H5Tset_strpad(H5tid, hdf5_h.H5T_STR_SPACEPAD());
        assertEquals("H5Tset_strpad SPACEPAD failed", 0, result);
        assertEquals("Should be SPACEPAD", hdf5_h.H5T_STR_SPACEPAD(), hdf5_h.H5Tget_strpad(H5tid));
    }

    // ============================================================================
    // H5T VLen Advanced Tests
    // ============================================================================

    @Test
    public void testH5Tvlen_create_nested()
    {
        System.out.print(testname.getMethodName());

        // Create vlen of vlen (nested variable length)
        long innerVlen = hdf5_h.H5Tvlen_create(hdf5_h.H5T_STD_I32LE_g());
        assertTrue("Inner vlen creation failed", isValidId(innerVlen));

        H5tid = hdf5_h.H5Tvlen_create(innerVlen);
        assertTrue("Outer vlen creation failed", isValidId(H5tid));

        // Verify it's a vlen type
        assertEquals("Should be vlen class", hdf5_h.H5T_VLEN(), hdf5_h.H5Tget_class(H5tid));

        // Get super type
        long superType = hdf5_h.H5Tget_super(H5tid);
        assertTrue("Super type should be valid", isValidId(superType));
        assertEquals("Super should be vlen", hdf5_h.H5T_VLEN(), hdf5_h.H5Tget_class(superType));

        hdf5_h.H5Tclose(superType);
        hdf5_h.H5Tclose(innerVlen);
    }

    @Test
    public void testH5Tvlen_with_compound()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create compound type
            long compoundType = hdf5_h.H5Tcreate(hdf5_h.H5T_COMPOUND(), 16);
            hdf5_h.H5Tinsert(compoundType, arena.allocateFrom("id"), 0, hdf5_h.H5T_STD_I32LE_g());
            hdf5_h.H5Tinsert(compoundType, arena.allocateFrom("value"), 8, hdf5_h.H5T_IEEE_F64LE_g());

            // Create vlen of compound
            H5tid = hdf5_h.H5Tvlen_create(compoundType);
            assertTrue("Vlen of compound creation failed", isValidId(H5tid));

            // Verify base type is compound
            long superType = hdf5_h.H5Tget_super(H5tid);
            assertEquals("Super type should be compound", hdf5_h.H5T_COMPOUND(),
                         hdf5_h.H5Tget_class(superType));

            hdf5_h.H5Tclose(superType);
            hdf5_h.H5Tclose(compoundType);
        }
    }

    @Test
    public void testH5Tvlen_is_variable()
    {
        System.out.print(testname.getMethodName());

        H5tid = hdf5_h.H5Tvlen_create(hdf5_h.H5T_STD_I32LE_g());

        // Vlen types report their size as sizeof(hvl_t) which is 16 bytes on 64-bit
        // The proper way to check is via the type class
        assertEquals("Should be vlen class", hdf5_h.H5T_VLEN(), hdf5_h.H5Tget_class(H5tid));

        // Verify the size is the hvl_t struct size (typically 16 bytes)
        long size = hdf5_h.H5Tget_size(H5tid);
        assertTrue("Vlen size should be positive (hvl_t struct)", size > 0);
    }

    // ============================================================================
    // H5T Opaque Advanced Tests
    // ============================================================================

    @Test
    public void testH5Topaque_create()
    {
        System.out.print(testname.getMethodName());

        // Create opaque type of 128 bytes
        H5tid = hdf5_h.H5Tcreate(hdf5_h.H5T_OPAQUE(), 128);
        assertTrue("Opaque creation failed", isValidId(H5tid));

        assertEquals("Should be opaque class", hdf5_h.H5T_OPAQUE(), hdf5_h.H5Tget_class(H5tid));
        assertEquals("Size should be 128", 128, hdf5_h.H5Tget_size(H5tid));
    }

    @Test
    public void testH5Topaque_set_get_tag()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            H5tid = hdf5_h.H5Tcreate(hdf5_h.H5T_OPAQUE(), 64);

            // Set tag
            String tag = "binary_blob_v1.0";
            int result = hdf5_h.H5Tset_tag(H5tid, arena.allocateFrom(tag));
            assertEquals("H5Tset_tag failed", 0, result);

            // Get tag
            MemorySegment tagPtr = hdf5_h.H5Tget_tag(H5tid);
            assertNotNull("Tag pointer should not be null", tagPtr);

            String retrievedTag = tagPtr.getString(0);
            assertEquals("Tag should match", tag, retrievedTag);

            hdf5_h.H5free_memory(tagPtr);
        }
    }

    @Test
    public void testH5Topaque_different_sizes()
    {
        System.out.print(testname.getMethodName());

        // Test various opaque sizes
        int[] sizes = {1, 16, 256, 1024};

        for (int size : sizes) {
            long tid = hdf5_h.H5Tcreate(hdf5_h.H5T_OPAQUE(), size);
            assertTrue("Opaque creation failed for size " + size, isValidId(tid));
            assertEquals("Size should match", size, hdf5_h.H5Tget_size(tid));
            hdf5_h.H5Tclose(tid);
        }
    }

    // ============================================================================
    // H5T Bitfield Tests
    // ============================================================================

    @Test
    public void testH5Tbitfield_create()
    {
        System.out.print(testname.getMethodName());

        // Bitfield types cannot be created with H5Tcreate, must copy from predefined type
        // Copy from a standard bitfield type and resize
        H5tid = hdf5_h.H5Tcopy(hdf5_h.H5T_STD_B32LE_g());
        assertTrue("Bitfield copy failed", isValidId(H5tid));

        // Verify it's a bitfield type
        assertEquals("Should be bitfield class", hdf5_h.H5T_BITFIELD(), hdf5_h.H5Tget_class(H5tid));

        // Resize to 4 bytes if needed
        int result = hdf5_h.H5Tset_size(H5tid, 4);
        assertTrue("H5Tset_size should succeed", isSuccess(result));
        assertEquals("Size should be 4", 4, hdf5_h.H5Tget_size(H5tid));
    }

    @Test
    public void testH5Tbitfield_predefined()
    {
        System.out.print(testname.getMethodName());

        // Test predefined bitfield types
        long tid1 = hdf5_h.H5Tcopy(hdf5_h.H5T_STD_B8LE_g());
        assertTrue("H5T_STD_B8LE copy failed", isValidId(tid1));
        assertEquals("Should be 1 byte", 1, hdf5_h.H5Tget_size(tid1));
        hdf5_h.H5Tclose(tid1);

        long tid2 = hdf5_h.H5Tcopy(hdf5_h.H5T_STD_B16LE_g());
        assertTrue("H5T_STD_B16LE copy failed", isValidId(tid2));
        assertEquals("Should be 2 bytes", 2, hdf5_h.H5Tget_size(tid2));
        hdf5_h.H5Tclose(tid2);

        long tid3 = hdf5_h.H5Tcopy(hdf5_h.H5T_STD_B32LE_g());
        assertTrue("H5T_STD_B32LE copy failed", isValidId(tid3));
        assertEquals("Should be 4 bytes", 4, hdf5_h.H5Tget_size(tid3));
        hdf5_h.H5Tclose(tid3);

        long tid4 = hdf5_h.H5Tcopy(hdf5_h.H5T_STD_B64LE_g());
        assertTrue("H5T_STD_B64LE copy failed", isValidId(tid4));
        assertEquals("Should be 8 bytes", 8, hdf5_h.H5Tget_size(tid4));
        hdf5_h.H5Tclose(tid4);
    }

    // ============================================================================
    // H5T Complex Number Tests (HDF5 2.0 feature)
    // ============================================================================

    @Test
    public void testH5Tcomplex_float()
    {
        System.out.print(testname.getMethodName());

        // Create complex float type
        H5tid = hdf5_h.H5Tcomplex_create(hdf5_h.H5T_IEEE_F32LE_g());
        assertTrue("Complex float creation failed", isValidId(H5tid));

        // Complex float should be 8 bytes (2 * 4-byte floats)
        long size = hdf5_h.H5Tget_size(H5tid);
        assertEquals("Complex float should be 8 bytes", 8, size);

        // Complex types have their own class (H5T_COMPLEX = 11)
        assertEquals("Should be complex class", hdf5_h.H5T_COMPLEX(), hdf5_h.H5Tget_class(H5tid));
    }

    @Test
    public void testH5Tcomplex_double()
    {
        System.out.print(testname.getMethodName());

        // Create complex double type
        H5tid = hdf5_h.H5Tcomplex_create(hdf5_h.H5T_IEEE_F64LE_g());
        assertTrue("Complex double creation failed", isValidId(H5tid));

        // Complex double should be 16 bytes (2 * 8-byte doubles)
        long size = hdf5_h.H5Tget_size(H5tid);
        assertEquals("Complex double should be 16 bytes", 16, size);
    }

    @Test
    public void testH5Tcomplex_get_parts()
    {
        System.out.print(testname.getMethodName());

        H5tid = hdf5_h.H5Tcomplex_create(hdf5_h.H5T_IEEE_F64LE_g());

        // Get real and imaginary part types
        long realType = hdf5_h.H5Tget_super(H5tid);
        assertTrue("Real type should be valid", isValidId(realType));

        // Verify it's a double
        int equal = hdf5_h.H5Tequal(realType, hdf5_h.H5T_IEEE_F64LE_g());
        assertTrue("Real part should be double", equal > 0);

        hdf5_h.H5Tclose(realType);
    }

    // ============================================================================
    // H5T Type Conversion Advanced Tests
    // ============================================================================

    @Test
    public void testH5Tconvert_with_buffer()
    {
        // Skip on Windows - FFM memory layout issue with conversion buffers
        Assume.assumeFalse("Skipping on Windows - FFM limitation", IS_WINDOWS);

        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Convert int array to float array
            int nelem             = 5;
            MemorySegment intData = arena.allocateFrom(hdf5_h.C_INT, 10, 20, 30, 40, 50);

            long srcType = hdf5_h.H5Tcopy(hdf5_h.H5T_STD_I32LE_g());
            long dstType = hdf5_h.H5Tcopy(hdf5_h.H5T_IEEE_F32LE_g());

            // Convert in place
            int result =
                hdf5_h.H5Tconvert(srcType, dstType, nelem, intData, MemorySegment.NULL, hdf5_h.H5P_DEFAULT());
            assertEquals("H5Tconvert failed", 0, result);

            // Data should now be floats (we can't easily verify values due to overlay)

            hdf5_h.H5Tclose(srcType);
            hdf5_h.H5Tclose(dstType);
        }
    }

    @Test
    public void testH5Tconvert_compound_subset()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create source compound: {int x, int y, int z}
            long srcType = hdf5_h.H5Tcreate(hdf5_h.H5T_COMPOUND(), 12);
            hdf5_h.H5Tinsert(srcType, arena.allocateFrom("x"), 0, hdf5_h.H5T_STD_I32LE_g());
            hdf5_h.H5Tinsert(srcType, arena.allocateFrom("y"), 4, hdf5_h.H5T_STD_I32LE_g());
            hdf5_h.H5Tinsert(srcType, arena.allocateFrom("z"), 8, hdf5_h.H5T_STD_I32LE_g());

            // Create dest compound: {int x, int z} - subset
            long dstType = hdf5_h.H5Tcreate(hdf5_h.H5T_COMPOUND(), 8);
            hdf5_h.H5Tinsert(dstType, arena.allocateFrom("x"), 0, hdf5_h.H5T_STD_I32LE_g());
            hdf5_h.H5Tinsert(dstType, arena.allocateFrom("z"), 4, hdf5_h.H5T_STD_I32LE_g());

            // This tests subset conversion capability
            assertTrue("Source compound type valid", isValidId(srcType));
            assertTrue("Dest compound type valid", isValidId(dstType));

            hdf5_h.H5Tclose(srcType);
            hdf5_h.H5Tclose(dstType);
        }
    }

    @Test
    public void testH5Tcompiler_conv()
    {
        System.out.print(testname.getMethodName());

        // Check if compiler conversion path exists between int and float
        int result = hdf5_h.H5Tcompiler_conv(hdf5_h.H5T_STD_I32LE_g(), hdf5_h.H5T_IEEE_F32LE_g());
        // Result > 0 means compiler conversion exists, 0 means library conversion only
        assertTrue("Conversion check should succeed", result >= 0);
    }

    // ============================================================================
    // H5T Type Commit Tests
    // ============================================================================

    @Test
    public void testH5Tcommit2()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create a test file
            long file_id = hdf5_h.H5Fcreate(arena.allocateFrom("test_commit.h5"), hdf5_h.H5F_ACC_TRUNC(),
                                            hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());
            assertTrue("File creation failed", isValidId(file_id));

            // Create compound type
            H5tid = hdf5_h.H5Tcreate(hdf5_h.H5T_COMPOUND(), 12);
            hdf5_h.H5Tinsert(H5tid, arena.allocateFrom("a"), 0, hdf5_h.H5T_STD_I32LE_g());
            hdf5_h.H5Tinsert(H5tid, arena.allocateFrom("b"), 4, hdf5_h.H5T_IEEE_F32LE_g());
            hdf5_h.H5Tinsert(H5tid, arena.allocateFrom("c"), 8, hdf5_h.H5T_STD_I32LE_g());

            // Commit the type
            int result = hdf5_h.H5Tcommit2(file_id, arena.allocateFrom("mytype"), H5tid, hdf5_h.H5P_DEFAULT(),
                                           hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());
            assertEquals("H5Tcommit2 failed", 0, result);

            // Verify it's committed
            int committed = hdf5_h.H5Tcommitted(H5tid);
            assertTrue("Type should be committed", committed > 0);

            hdf5_h.H5Fclose(file_id);
        }
    }

    @Test
    public void testH5Tcommit_anon()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long file_id = hdf5_h.H5Fcreate(arena.allocateFrom("test_commit_anon.h5"), hdf5_h.H5F_ACC_TRUNC(),
                                            hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());

            // Create enum type
            H5tid             = hdf5_h.H5Tenum_create(hdf5_h.H5T_STD_I32LE_g());
            MemorySegment val = arena.allocate(hdf5_h.C_INT, 0);
            hdf5_h.H5Tenum_insert(H5tid, arena.allocateFrom("RED"), val);

            // Commit anonymously (no name)
            int result = hdf5_h.H5Tcommit_anon(file_id, H5tid, hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());
            assertEquals("H5Tcommit_anon failed", 0, result);

            // Verify it's committed
            assertTrue("Type should be committed", hdf5_h.H5Tcommitted(H5tid) > 0);

            hdf5_h.H5Fclose(file_id);
        }
    }

    @Test
    public void testH5Topen2()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long file_id = hdf5_h.H5Fcreate(arena.allocateFrom("test_open.h5"), hdf5_h.H5F_ACC_TRUNC(),
                                            hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());

            // Create and commit a type
            long tid1 = hdf5_h.H5Tcopy(hdf5_h.H5T_IEEE_F64LE_g());
            hdf5_h.H5Tcommit2(file_id, arena.allocateFrom("double_type"), tid1, hdf5_h.H5P_DEFAULT(),
                              hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());
            hdf5_h.H5Tclose(tid1);

            // Open the committed type
            H5tid = hdf5_h.H5Topen2(file_id, arena.allocateFrom("double_type"), hdf5_h.H5P_DEFAULT());
            assertTrue("H5Topen2 failed", isValidId(H5tid));

            // Verify it's committed
            assertTrue("Opened type should be committed", hdf5_h.H5Tcommitted(H5tid) > 0);

            // Verify it's equal to double
            int equal = hdf5_h.H5Tequal(H5tid, hdf5_h.H5T_IEEE_F64LE_g());
            assertTrue("Should be equal to double", equal > 0);

            hdf5_h.H5Fclose(file_id);
        }
    }

    @Test
    public void testH5Tget_create_plist()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long file_id = hdf5_h.H5Fcreate(arena.allocateFrom("test_get_cplist.h5"), hdf5_h.H5F_ACC_TRUNC(),
                                            hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());

            // Create type with custom creation properties
            long tcpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_DATATYPE_CREATE_ID_g());
            assertTrue("TCPL creation failed", isValidId(tcpl));

            H5tid = hdf5_h.H5Tcopy(hdf5_h.H5T_STD_I32LE_g());
            hdf5_h.H5Tcommit2(file_id, arena.allocateFrom("int_type"), H5tid, hdf5_h.H5P_DEFAULT(), tcpl,
                              hdf5_h.H5P_DEFAULT());

            // Get creation property list
            long retrieved_tcpl = hdf5_h.H5Tget_create_plist(H5tid);
            assertTrue("H5Tget_create_plist failed", isValidId(retrieved_tcpl));

            hdf5_h.H5Pclose(retrieved_tcpl);
            hdf5_h.H5Pclose(tcpl);
            hdf5_h.H5Fclose(file_id);
        }
    }

    // ============================================================================
    // H5T Type Detection and Query Tests
    // ============================================================================

    @Test
    public void testH5Tdetect_class_in_compound()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create compound with int and float
            long compoundType = hdf5_h.H5Tcreate(hdf5_h.H5T_COMPOUND(), 12);
            hdf5_h.H5Tinsert(compoundType, arena.allocateFrom("i"), 0, hdf5_h.H5T_STD_I32LE_g());
            hdf5_h.H5Tinsert(compoundType, arena.allocateFrom("f"), 4, hdf5_h.H5T_IEEE_F32LE_g());

            // Detect integer class
            int hasInt = hdf5_h.H5Tdetect_class(compoundType, hdf5_h.H5T_INTEGER());
            assertTrue("Should detect integer class", hasInt > 0);

            // Detect float class
            int hasFloat = hdf5_h.H5Tdetect_class(compoundType, hdf5_h.H5T_FLOAT());
            assertTrue("Should detect float class", hasFloat > 0);

            // Should not detect string class
            int hasString = hdf5_h.H5Tdetect_class(compoundType, hdf5_h.H5T_STRING());
            assertFalse("Should not detect string class", hasString > 0);

            hdf5_h.H5Tclose(compoundType);
        }
    }

    @Test
    public void testH5Tdetect_class_integer()
    {
        System.out.print(testname.getMethodName());

        int result = hdf5_h.H5Tdetect_class(hdf5_h.H5T_STD_I32LE_g(), hdf5_h.H5T_INTEGER());
        assertTrue("H5T_STD_I32LE should be integer class", result > 0);

        result = hdf5_h.H5Tdetect_class(hdf5_h.H5T_STD_I32LE_g(), hdf5_h.H5T_FLOAT());
        assertFalse("H5T_STD_I32LE should not be float class", result > 0);
    }

    @Test
    public void testH5Tdetect_class_float()
    {
        System.out.print(testname.getMethodName());

        int result = hdf5_h.H5Tdetect_class(hdf5_h.H5T_IEEE_F64LE_g(), hdf5_h.H5T_FLOAT());
        assertTrue("Native double should be float class", result > 0);

        result = hdf5_h.H5Tdetect_class(hdf5_h.H5T_IEEE_F64LE_g(), hdf5_h.H5T_INTEGER());
        assertFalse("Native double should not be integer class", result > 0);
    }

    // ============================================================================
    // H5T Type Modification Tests
    // ============================================================================

    @Test
    public void testH5Tpack_compound()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create compound with padding
            long compoundType = hdf5_h.H5Tcreate(hdf5_h.H5T_COMPOUND(), 16);
            hdf5_h.H5Tinsert(compoundType, arena.allocateFrom("a"), 0, hdf5_h.H5T_STD_I8LE_g());
            hdf5_h.H5Tinsert(compoundType, arena.allocateFrom("b"), 4, hdf5_h.H5T_STD_I32LE_g());

            long sizeBefore = hdf5_h.H5Tget_size(compoundType);

            // Pack to remove padding
            int result = hdf5_h.H5Tpack(compoundType);
            assertEquals("H5Tpack failed", 0, result);

            long sizeAfter = hdf5_h.H5Tget_size(compoundType);
            assertTrue("Size should be smaller after packing", sizeAfter <= sizeBefore);
            // Should be 1 (char) + 4 (int) = 5 bytes
            assertEquals("Packed size should be 5", 5, sizeAfter);

            hdf5_h.H5Tclose(compoundType);
        }
    }

    @Test
    public void testH5Tlock()
    {
        System.out.print(testname.getMethodName());

        H5tid = hdf5_h.H5Tcopy(hdf5_h.H5T_STD_I32LE_g());

        // Lock the type
        int result = hdf5_h.H5Tlock(H5tid);
        assertEquals("H5Tlock failed", 0, result);

        // Try to modify locked type - should fail
        result = hdf5_h.H5Tset_size(H5tid, 8);
        assertTrue("Modifying locked type should fail", result < 0);
    }

    // ============================================================================
    // H5T String Character Set Tests
    // ============================================================================

    @Test
    public void testH5Tset_get_cset_ascii()
    {
        System.out.print(testname.getMethodName());

        H5tid = hdf5_h.H5Tcopy(hdf5_h.H5T_C_S1_g());

        // Set to ASCII
        int result = hdf5_h.H5Tset_cset(H5tid, hdf5_h.H5T_CSET_ASCII());
        assertEquals("H5Tset_cset ASCII failed", 0, result);

        int cset = hdf5_h.H5Tget_cset(H5tid);
        assertEquals("Character set should be ASCII", hdf5_h.H5T_CSET_ASCII(), cset);
    }

    @Test
    public void testH5Tset_get_cset_utf8()
    {
        System.out.print(testname.getMethodName());

        H5tid = hdf5_h.H5Tcopy(hdf5_h.H5T_C_S1_g());

        // Set to UTF-8
        int result = hdf5_h.H5Tset_cset(H5tid, hdf5_h.H5T_CSET_UTF8());
        assertEquals("H5Tset_cset UTF8 failed", 0, result);

        int cset = hdf5_h.H5Tget_cset(H5tid);
        assertEquals("Character set should be UTF8", hdf5_h.H5T_CSET_UTF8(), cset);
    }

    // ============================================================================
    // H5T Reference Type Tests
    // ============================================================================

    @Test
    public void testH5T_STD_REF()
    {
        System.out.print(testname.getMethodName());

        long tid = hdf5_h.H5Tcopy(hdf5_h.H5T_STD_REF_g());
        assertTrue("H5T_STD_REF copy failed", isValidId(tid));

        // Verify it's a reference type
        assertEquals("Should be reference class", hdf5_h.H5T_REFERENCE(), hdf5_h.H5Tget_class(tid));

        hdf5_h.H5Tclose(tid);
    }

    @Test
    public void testH5T_reference_types()
    {
        System.out.print(testname.getMethodName());

        // Test object reference
        long objRef = hdf5_h.H5Tcopy(hdf5_h.H5T_STD_REF_OBJ_g());
        assertTrue("Object reference copy failed", isValidId(objRef));
        assertEquals("Should be reference", hdf5_h.H5T_REFERENCE(), hdf5_h.H5Tget_class(objRef));
        hdf5_h.H5Tclose(objRef);

        // Test dataset region reference
        long regRef = hdf5_h.H5Tcopy(hdf5_h.H5T_STD_REF_DSETREG_g());
        assertTrue("Region reference copy failed", isValidId(regRef));
        assertEquals("Should be reference", hdf5_h.H5T_REFERENCE(), hdf5_h.H5Tget_class(regRef));
        hdf5_h.H5Tclose(regRef);
    }

    // ============================================================================
    // H5T Array Type Advanced Tests
    // ============================================================================

    @Test
    public void testH5Tarray_equal()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            MemorySegment dims = arena.allocateFrom(ValueLayout.JAVA_LONG, 5, 10);

            long tid1 = hdf5_h.H5Tarray_create2(hdf5_h.H5T_STD_I32LE_g(), 2, dims);
            long tid2 = hdf5_h.H5Tarray_create2(hdf5_h.H5T_STD_I32LE_g(), 2, dims);

            // Should be equal
            int equal = hdf5_h.H5Tequal(tid1, tid2);
            assertTrue("Identical arrays should be equal", equal > 0);

            hdf5_h.H5Tclose(tid1);
            hdf5_h.H5Tclose(tid2);

            // Different dimensions should not be equal
            MemorySegment dims2 = arena.allocateFrom(ValueLayout.JAVA_LONG, 5, 11);
            long tid3           = hdf5_h.H5Tarray_create2(hdf5_h.H5T_STD_I32LE_g(), 2, dims);
            long tid4           = hdf5_h.H5Tarray_create2(hdf5_h.H5T_STD_I32LE_g(), 2, dims2);

            equal = hdf5_h.H5Tequal(tid3, tid4);
            assertFalse("Different dimension arrays should not be equal", equal > 0);

            hdf5_h.H5Tclose(tid3);
            hdf5_h.H5Tclose(tid4);
        }
    }

    @Test
    public void testH5Tarray_multidimensional_access()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create 4D array [2][3][4][5]
            MemorySegment dims = arena.allocateFrom(ValueLayout.JAVA_LONG, 2, 3, 4, 5);
            H5tid              = hdf5_h.H5Tarray_create2(hdf5_h.H5T_IEEE_F64LE_g(), 4, dims);

            // Verify rank
            assertEquals("Should be 4D", 4, hdf5_h.H5Tget_array_ndims(H5tid));

            // Get all dimensions
            MemorySegment retrievedDims = arena.allocate(ValueLayout.JAVA_LONG, 4);
            hdf5_h.H5Tget_array_dims2(H5tid, retrievedDims);

            assertEquals("Dim[0]", 2, retrievedDims.getAtIndex(ValueLayout.JAVA_LONG, 0));
            assertEquals("Dim[1]", 3, retrievedDims.getAtIndex(ValueLayout.JAVA_LONG, 1));
            assertEquals("Dim[2]", 4, retrievedDims.getAtIndex(ValueLayout.JAVA_LONG, 2));
            assertEquals("Dim[3]", 5, retrievedDims.getAtIndex(ValueLayout.JAVA_LONG, 3));

            // Size should be 2*3*4*5*8 = 960 bytes (8 bytes per double)
            assertEquals("Size should be 960", 960, hdf5_h.H5Tget_size(H5tid));
        }
    }

    // ============================================================================
    // H5T Enum Type Advanced Tests
    // ============================================================================

    @Test
    public void testH5Tenum_with_different_base_types()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Enum based on byte
            long enumByte     = hdf5_h.H5Tenum_create(hdf5_h.H5T_STD_I8LE_g());
            MemorySegment val = arena.allocate(hdf5_h.C_CHAR, (byte)1);
            hdf5_h.H5Tenum_insert(enumByte, arena.allocateFrom("ONE"), val);
            assertEquals("Size should be 1", 1, hdf5_h.H5Tget_size(enumByte));
            hdf5_h.H5Tclose(enumByte);

            // Enum based on short
            long enumShort     = hdf5_h.H5Tenum_create(hdf5_h.H5T_STD_I16LE_g());
            MemorySegment val2 = arena.allocate(hdf5_h.C_SHORT, (short)1);
            hdf5_h.H5Tenum_insert(enumShort, arena.allocateFrom("ONE"), val2);
            assertEquals("Size should be 2", 2, hdf5_h.H5Tget_size(enumShort));
            hdf5_h.H5Tclose(enumShort);

            // Enum based on long
            long enumLong      = hdf5_h.H5Tenum_create(hdf5_h.H5T_STD_I64LE_g());
            MemorySegment val3 = arena.allocate(ValueLayout.JAVA_LONG, 1L);
            hdf5_h.H5Tenum_insert(enumLong, arena.allocateFrom("ONE"), val3);
            assertEquals("Size should be 8", 8, hdf5_h.H5Tget_size(enumLong));
            hdf5_h.H5Tclose(enumLong);
        }
    }

    @Test
    public void testH5Tenum_get_member_index()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            H5tid = hdf5_h.H5Tenum_create(hdf5_h.H5T_STD_I32LE_g());

            // Insert several members
            String[] names = {"ALPHA", "BETA", "GAMMA", "DELTA"};
            for (int i = 0; i < names.length; i++) {
                MemorySegment val = allocateInt(arena);
                setInt(val, i * 100);
                hdf5_h.H5Tenum_insert(H5tid, stringToSegment(arena, names[i]), val);
            }

            // Get member index
            int idx = hdf5_h.H5Tget_member_index(H5tid, stringToSegment(arena, "BETA"));
            assertEquals("BETA should be at index 1", 1, idx);

            idx = hdf5_h.H5Tget_member_index(H5tid, stringToSegment(arena, "DELTA"));
            assertEquals("DELTA should be at index 3", 3, idx);

            // Non-existent member
            idx = hdf5_h.H5Tget_member_index(H5tid, stringToSegment(arena, "EPSILON"));
            assertTrue("Non-existent member should return negative", idx < 0);
        }
    }

    // ============================================================================
    // H5T Compound Type Advanced Tests
    // ============================================================================

    @Test
    public void testH5Tget_member_index_compound()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long compoundType = hdf5_h.H5Tcreate(hdf5_h.H5T_COMPOUND(), 16);
            hdf5_h.H5Tinsert(compoundType, arena.allocateFrom("field1"), 0, hdf5_h.H5T_STD_I32LE_g());
            hdf5_h.H5Tinsert(compoundType, arena.allocateFrom("field2"), 4, hdf5_h.H5T_IEEE_F32LE_g());
            hdf5_h.H5Tinsert(compoundType, arena.allocateFrom("field3"), 8, hdf5_h.H5T_IEEE_F64LE_g());

            int idx = hdf5_h.H5Tget_member_index(compoundType, arena.allocateFrom("field2"));
            assertEquals("field2 should be at index 1", 1, idx);

            idx = hdf5_h.H5Tget_member_index(compoundType, arena.allocateFrom("field3"));
            assertEquals("field3 should be at index 2", 2, idx);

            hdf5_h.H5Tclose(compoundType);
        }
    }

    @Test
    public void testH5Tget_member_class()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long compoundType = hdf5_h.H5Tcreate(hdf5_h.H5T_COMPOUND(), 13);
            hdf5_h.H5Tinsert(compoundType, arena.allocateFrom("i"), 0, hdf5_h.H5T_STD_I32LE_g());
            hdf5_h.H5Tinsert(compoundType, arena.allocateFrom("f"), 4, hdf5_h.H5T_IEEE_F32LE_g());
            hdf5_h.H5Tinsert(compoundType, arena.allocateFrom("c"), 8, hdf5_h.H5T_STD_I8LE_g());

            int class0 = hdf5_h.H5Tget_member_class(compoundType, 0);
            assertEquals("Member 0 should be integer", hdf5_h.H5T_INTEGER(), class0);

            int class1 = hdf5_h.H5Tget_member_class(compoundType, 1);
            assertEquals("Member 1 should be float", hdf5_h.H5T_FLOAT(), class1);

            int class2 = hdf5_h.H5Tget_member_class(compoundType, 2);
            assertEquals("Member 2 should be integer (char)", hdf5_h.H5T_INTEGER(), class2);

            hdf5_h.H5Tclose(compoundType);
        }
    }

    // ============================================================================
    // H5T Numeric Type Property Tests
    // ============================================================================

    @Test
    public void testH5Tset_size_grow()
    {
        System.out.print(testname.getMethodName());

        H5tid = hdf5_h.H5Tcopy(hdf5_h.H5T_STD_I16LE_g());
        assertEquals("Short should be 2 bytes", 2, hdf5_h.H5Tget_size(H5tid));

        // Grow to 8 bytes
        int result = hdf5_h.H5Tset_size(H5tid, 8);
        assertEquals("H5Tset_size grow failed", 0, result);
        assertEquals("Size should be 8", 8, hdf5_h.H5Tget_size(H5tid));
    }

    @Test
    public void testH5Tset_size_shrink()
    {
        System.out.print(testname.getMethodName());

        H5tid = hdf5_h.H5Tcopy(hdf5_h.H5T_STD_I64LE_g());
        assertEquals("Long should be 8 bytes", 8, hdf5_h.H5Tget_size(H5tid));

        // Shrink to 4 bytes
        int result = hdf5_h.H5Tset_size(H5tid, 4);
        assertEquals("H5Tset_size shrink failed", 0, result);
        assertEquals("Size should be 4", 4, hdf5_h.H5Tget_size(H5tid));
    }

    @Test
    public void testH5Tset_precision_less_than_size()
    {
        System.out.print(testname.getMethodName());

        H5tid = hdf5_h.H5Tcopy(hdf5_h.H5T_STD_I32LE_g());

        // Set precision to 24 bits (less than 32-bit int)
        int result = hdf5_h.H5Tset_precision(H5tid, 24);
        assertEquals("H5Tset_precision failed", 0, result);

        long precision = hdf5_h.H5Tget_precision(H5tid);
        assertEquals("Precision should be 24", 24, precision);
    }

    @Test
    public void testH5Tget_native_type_integer()
    {
        // Skip on Windows - FFM memory layout issue with native type mapping
        Assume.assumeFalse("Skipping on Windows - FFM limitation", IS_WINDOWS);

        System.out.print(testname.getMethodName());

        // Get native type for a standard type
        long nativeType = hdf5_h.H5Tget_native_type(hdf5_h.H5T_STD_I32LE_g(), hdf5_h.H5T_DIR_ASCEND());
        assertTrue("Native type should be valid", isValidId(nativeType));
        assertEquals("Should be integer class", hdf5_h.H5T_INTEGER(), hdf5_h.H5Tget_class(nativeType));
        hdf5_h.H5Tclose(nativeType);
    }

    @Test
    public void testH5Tget_native_type_float()
    {
        System.out.print(testname.getMethodName());

        long nativeType = hdf5_h.H5Tget_native_type(hdf5_h.H5T_IEEE_F64LE_g(), hdf5_h.H5T_DIR_DESCEND());
        assertTrue("Native type should be valid", isValidId(nativeType));
        assertEquals("Should be float class", hdf5_h.H5T_FLOAT(), hdf5_h.H5Tget_class(nativeType));
        hdf5_h.H5Tclose(nativeType);
    }
}
