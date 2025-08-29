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

package test;

import static org.junit.Assert.assertTrue;

import hdf.hdf5lib.H5;
import hdf.hdf5lib.exceptions.HDF5LibraryException;

import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

public class TestH5Tparams {
    @Rule
    public TestName testname = new TestName();

    @Before
    public void checkOpenIDs()
    {
        System.out.print(testname.getMethodName());
    }
    @After
    public void nextTestName()
    {
        System.out.println();
    }

    @Test
    public void testH5Tclose_invalid() throws Throwable
    {
        long tid = H5Tclose(-1);
        assertTrue("testH5Tclose_invalid", tid == 0);
    }

    @Test
    public void testH5Tcopy_invalid() throws Throwable
    {
        assertTrue("testH5Tcopy_invalid", H5Tcopy(-1) < 0);
    }

    @Test
    public void testH5Tequal_invalid() throws Throwable
    {
        assertTrue("testH5Tequal_invalid", H5Tequal(-1, -1) < 0);
    }

    @Test
    public void testH5Tlock_invalid() throws Throwable
    {
        assertTrue("testH5Tlock_invalid", H5Tlock(-1) < 0);
    }

    @Test
    public void testH5Tget_class_invalid() throws Throwable
    {
        assertTrue("testH5Tget_class_invalid", H5Tget_class(-1) < 0);
    }

    @Test
    public void testH5Tget_size_invalid() throws Throwable
    {
        assertTrue("testH5Tget_size_invalid", H5Tget_size(-1) < 0);
    }

    @Test
    public void testH5Tset_size_invalid() throws Throwable
    {
        assertTrue("testH5Tset_size_invalid", H5Tset_size(-1) < 0);
    }

    @Test
    public void testH5Tget_order_invalid() throws Throwable
    {
        assertTrue("testH5Tget_order_invalid", H5Tget_order(-1) < 0);
    }

    @Test
    public void testH5Tset_order_invalid() throws Throwable
    {
        assertTrue("testH5Tset_order_invalid", H5Tset_order(-1, 0) < 0);
    }

    @Test
    public void testH5Tget_precision_invalid() throws Throwable
    {
        assertTrue("testH5Tget_precision_invalid", H5Tget_precision(-1) < 0);
    }

    @Test
    public void testH5Tget_precision_long_invalid() throws Throwable
    {
        assertTrue("testH5Tget_precision_long_invalid", H5Tget_precision_long(-1) < 0);
    }

    @Test
    public void testH5Tset_precision_invalid() throws Throwable
    {
        assertTrue("testH5Tset_precision_invalid", H5Tset_precision(-1) < 0);
    }

    @Test
    public void testH5Tget_offset_invalid() throws Throwable
    {
        assertTrue("testH5Tget_offset_invalid", H5Tget_offset(-1) < 0);
    }

    @Test
    public void testH5Tset_offset_invalid() throws Throwable
    {
        assertTrue("testH5Tset_offset_invalid", H5Tset_offset(-1, 0) < 0);
    }

    @Test
    public void testH5Tcreate_invalid() throws Throwable
    {
        assertTrue("testH5Tcreate_invalid", H5Tcreate(-1, (long)0) < 0);
    }

    @Test
    public void testH5Topen_null() throws Throwable
    {
        assertTrue("testH5Topen_null", H5Topen(-1, null, 0) < 0);
    }

    @Test
    public void testH5Topen_invalid() throws Throwable
    {
        long tid = H5I_INVALID_HID();
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment filename_segment = arena.allocateFrom("Bogus");
            tid                            = H5Topen(-1, filename_segment, 0);
        }
        assertTrue("H5Topen", tid < 0);
    }

    @Test
    public void testH5Tcommit_null() throws Throwable
    {
        assertTrue("testH5Tcommit_null", H5Tcommit2(-1, null, 0, -1, -1, -1) < 0);
    }

    @Test
    public void testH5Tcommit_invalid() throws Throwable
    {
        int status = -1;
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment filename_segment = arena.allocateFrom("Bogus");
            status                         = H5Tcommit2(-1, filename_segment, -1, -1, -1, -1);
        }
        assertTrue("testH5Tcommit_invalid", status < 0);
    }

    @Test
    public void testH5Tget_pad_null() throws Throwable
    {
        assertTrue("testH5Tget_pad_null", H5Tget_pad(-1, null) < 0);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Tget_pad_invalid() throws Throwable
    {
        int[] pad = new int[2];
        H5Tget_pad(-1, pad);
    }

    @Test
    public void testH5Tset_pad_invalid() throws Throwable
    {
        assertTrue("testH5Tset_pad_invalid", H5Tset_pad(-1, -1, -1) < 0);
    }

    @Test
    public void testH5Tget_sign_invalid() throws Throwable
    {
        assertTrue("testH5Tget_sign_invalid", H5Tget_sign(-1) < 0);
    }

    @Test
    public void testH5Tset_sign_invalid() throws Throwable
    {
        assertTrue("testH5Tset_sign_invalid", H5Tset_sign(-1, 0) < 0);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Tget_fields_null() throws Throwable
    {
        H5Tget_fields(-1, (long[])null);
    }

    @Test(expected = HDF5FunctionArgumentException.class)
    public void testH5Tget_fields_length_invalid() throws Throwable
    {
        long[] fields = new long[2];
        H5Tget_fields(-1, fields);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Tget_fields_invalid() throws Throwable
    {
        long[] fields = new long[5];
        H5Tget_fields(-1, fields);
    }

    @Test
    public void testH5Tset_fields_invalid() throws Throwable
    {
        assertTrue("testH5Tset_fields_invalid", H5Tset_fields(-1, -1, -1, -1, -1, -1) < 0);
    }

    @Test
    public void testH5Tget_ebias_invalid() throws Throwable
    {
        assertTrue("testH5Tget_ebias_invalid", H5Tget_ebias(-1) < 0);
    }

    @Test
    public void testH5Tget_ebias_long_invalid() throws Throwable
    {
        assertTrue("testH5Tget_ebias_long_invalid", H5Tget_ebias_long(-1) < 0);
    }

    @Test
    public void testH5Tset_ebias_invalid() throws Throwable
    {
        assertTrue("testH5Tset_ebias_invalid", H5Tset_ebias(-1, 0) < 0);
    }

    @Test
    public void testH5Tget_norm_invalid() throws Throwable
    {
        assertTrue("testH5Tget_norm_invalid", H5Tget_norm(-1) < 0);
    }

    @Test
    public void testH5Tset_norm_invalid() throws Throwable
    {
        assertTrue("testH5Tset_norm_invalid", H5Tset_norm(-1, 0) < 0);
    }

    @Test
    public void testH5Tget_inpad_invalid() throws Throwable
    {
        assertTrue("testH5Tget_inpad_invalid", H5Tget_inpad(-1) < 0);
    }

    @Test
    public void testH5Tset_inpad_invalid() throws Throwable
    {
        H5Tset_inpad(-1, 0);
        assertTrue("testH5Tset_inpad_invalid", H5Tset_inpad(-1, 0) < 0);
    }

    @Test
    public void testH5Tget_cset_invalid() throws Throwable
    {
        H5Tget_cset(-1);
        assertTrue("testH5Tget_cset_invalid", H5Tget_cset(-1) < 0);
    }

    @Test
    public void testH5Tset_cset_invalid() throws Throwable
    {
        assertTrue("testH5Tset_cset_invalid", H5Tset_cset(-1, 0) < 0);
    }

    @Test
    public void testH5Tget_strpad_invalid() throws Throwable
    {
        assertTrue("testH5Tget_strpad_invalid", H5Tget_strpad(-1, 0) < 0);
    }

    @Test
    public void testH5Tset_strpad_invalid() throws Throwable
    {
        assertTrue("testH5Tset_strpad_invalid", H5Tset_strpad(-1, 0) < 0);
    }

    @Test
    public void testH5Tget_nmembers_invalid() throws Throwable
    {
        assertTrue("testH5Tget_nmembers_invalid", H5Tget_nmembers(-1) < 0);
    }

    @Test
    public void testH5Tget_member_index_null() throws Throwable
    {
        assertTrue("testH5Tget_member_index_null", H5Tget_member_index(-1, null) < 0);
    }

    @Test
    public void testH5Tget_member_index_invalid() throws Throwable
    {
        int status = -1;
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the enum bytes
            MemorySegment name_segment = arena.allocateFrom("bogus");
            status                     = H5Tget_member_index(-1, name_segment);
        }
        assertTrue("", (-1) < 0);
    }

    @Test
    public void testH5Tget_member_type_invalid() throws Throwable
    {
        assertTrue("testH5Tget_member_type_invalid", H5Tget_member_type(-1, -1) < 0);
    }

    @Test
    public void testH5Tget_member_class_invalid() throws Throwable
    {
        assertTrue("testH5Tget_member_class_invalid", H5Tget_member_class(-1, -1) < 0);
    }

    @Test
    public void testH5Tinsert_null() throws Throwable
    {
        assertTrue("testH5Tinsert_null", H5Tinsert(-1, null, 0, 0) < 0);
    }

    @Test
    public void testH5Tinsert_invalid() throws Throwable
    {
        int status = -1;
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the enum bytes
            MemorySegment name_segment = arena.allocateFrom("bogus");
            status                     = H5Tinsert(-1, name_segment, 0, 0);
        }
        assertTrue("", (-1) < 0);
    }

    @Test
    public void testH5Tpack_invalid() throws Throwable
    {
        assertTrue("testH5Tpack_invalid", H5Tpack(-1) < 0);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Treclaim_invalid() throws Throwable
    {
        byte[] buf = new byte[2];
        H5Treclaim(-1, -1, -1, buf);
    }

    @Test
    public void testH5Treclaim_null() throws Throwable
    {
        assertTrue("testH5Treclaim_null", H5Treclaim(-1, -1, -1, null) < 0);
    }

    @Test
    public void testH5Tvlen_create_invalid() throws Throwable
    {
        assertTrue("testH5Tvlen_create_invalid", H5Tvlen_create(-1) < 0);
    }

    @Test
    public void testH5Tset_tag_null() throws Throwable
    {
        assertTrue("testH5Tset_tag_null", H5Tset_tag(-1, null) < 0);
    }

    @Test
    public void testH5Tset_tag_invalid() throws Throwable
    {
        int status = -1;
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the enum bytes
            MemorySegment name_segment = arena.allocateFrom("bogus");
            status                     = H5Tset_tag(-1, name_segment);
        }
        assertTrue("testH5Tset_tag_invalid", status < 0);
    }

    @Test
    public void testH5Tget_super_invalid() throws Throwable
    {
        assertTrue("testH5Tget_super_invalid", H5Tget_super(-1) < 0);
    }

    @Test
    public void testH5Tenum_create_invalid() throws Throwable
    {
        assertTrue("testH5Tenum_create_invalid", H5Tenum_create(-1) < 0);
    }

    @Test
    public void testH5Tenum_insert_name_null() throws Throwable
    {
        assertTrue("testH5Tenum_insert_name_null", H5Tenum_insert(-1, null, null) < 0);
    }

    @Test
    public void testH5Tenum_insert_null() throws Throwable
    {
        int status = -1;
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the enum bytes
            MemorySegment name_segment = arena.allocateFrom("bogus");
            status                     = H5Tenum_insert(-1, name_segment, null);
        }
        assertTrue("testH5Tenum_insert_null", status < 0);
    }

    @Test
    public void testH5Tenum_insert_invalid() throws Throwable
    {
        int status      = -1;
        byte[] enumtype = new byte[2];
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the enum bytes
            MemorySegment name_segment = arena.allocateFrom("bogus");
            MemorySegment enum_segment = arena.allocate(ValueLayout.JAVA_BYTE);
            // Set the int value
            enum_segment.set(ValueLayout.JAVA_BYTE, 0, enumtype[0]);
            status = H5Tenum_insert(-1, name_segment, enum_segment);
        }
        assertTrue("testH5Tenum_insert_invalid", status < 0);
    }

    @Test
    public void testH5Tenum_nameof_invalid_size() throws Throwable
    {
        assertTrue("testH5Tenum_nameof_invalid_size", H5Tenum_nameof(-1, null, -1) < 0);
    }

    @Test
    public void testH5Tenum_nameof_value_null() throws Throwable
    {
        assertTrue("testH5Tenum_nameof_value_null", H5Tenum_nameof(-1, null, 1) < 0);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Tenum_nameof_invalid() throws Throwable
    {
        byte[] btype = new byte[2];
        H5Tenum_nameof(-1, btype, 1);
        int status      = -1;
        byte[] enumtype = new byte[2];
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the enum bytes
            MemorySegment enum_segment = arena.allocate(ValueLayout.JAVA_BYTE);
            // Set the int value
            enum_segment.set(ValueLayout.JAVA_BYTE, 0, enumtype[0]);
            status = H5Tenum_nameof(-1, enum_segment, 1);
        }
        assertTrue("testH5Tenum_insert_invalid", status < 0);
    }

    @Test
    public void testH5Tenum_valueof_name_null() throws Throwable
    {
        assertTrue("testH5Tenum_valueof_name_null", H5Tenum_valueof(-1, null, (byte[])null) < 0);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Tenum_valueof_null() throws Throwable
    {
        int status = -1;
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the enum bytes
            MemorySegment name_segment = arena.allocateFrom("bogus");
            status                     = H5Tenum_valueof(-1, name_segment, (byte[])null);
        }
        assertTrue("testH5Tenum_valueof_null", status < 0);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Tenum_valueof_invalid() throws Throwable
    {
        byte[] btype = new byte[2];
        int status   = -1;
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the enum bytes
            MemorySegment name_segment = arena.allocateFrom("bogus");
            status                     = H5Tenum_valueof(-1, name_segment, );
        }
        assertTrue("testH5Tenum_valueof_invalid", status < 0);
        H5Tenum_valueof(-1, "bogus", btype);
    }

    @Test
    public void testH5Tget_member_value_null() throws Throwable
    {
        assertTrue("testH5Tget_member_value_null", H5Tget_member_value(-1, -1, (byte[])null) < 0);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Tget_member_value_invalid() throws Throwable
    {
        byte[] btype = new byte[2];
        H5Tget_member_value(-1, -1, btype);
    }

    @Test(expected = HDF5FunctionArgumentException.class)
    public void testH5Tarray_create_invalid() throws Throwable
    {
        assertTrue("testH5Tarray_create_invalid", H5Tarray_create(-1, -1, null) < 0);
    }

    @Test
    public void testH5Tarray_create_value_null() throws Throwable
    {
        assertTrue("testH5Tarray_create_value_null", H5Tarray_create(-1, 1, null) < 0);
    }

    @Test
    public void testH5Tget_array_ndims_invalid() throws Throwable
    {
        assertTrue("testH5Tget_array_ndims_invalid", H5Tget_array_ndims(-1) < 0);
    }

    @Test
    public void testH5Tget_array_dims_null() throws Throwable
    {
        assertTrue("testH5Tget_array_dims_null", H5Tget_array_dims(-1, null) < 0);
    }

    @Test
    public void testH5Tget_native_type_invalid() throws Throwable
    {
        assertTrue("testH5Tget_native_type_invalid", H5Tget_native_type(-1) < 0);
    }

    @Test
    public void testH5Tflush_invalid() throws Throwable
    {
        assertTrue("testH5Tflush_invalid", H5Tflush(-1) < 0);
    }

    @Test
    public void testH5Trefresh_invalid() throws Throwable
    {
        assertTrue("testH5Trefresh_invalid", H5Trefresh(-1) < 0);
    }
}
