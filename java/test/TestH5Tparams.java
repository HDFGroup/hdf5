/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
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
    @Rule public TestName testname = new TestName();

    @Before
    public void checkOpenIDs() {
        assertTrue("H5 open ids is 0",H5.getOpenIDCount()==0);
        System.out.print(testname.getMethodName());
    }
    @After
    public void nextTestName() {
        System.out.println();
    }

    @Test//(expected = HDF5LibraryException.class)
    public void testH5Tclose_invalid() throws Throwable {
        long tid = H5.H5Tclose(-1);
        assertTrue(tid == 0);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Tcopy_invalid() throws Throwable {
        H5.H5Tcopy(-1);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Tequal_invalid() throws Throwable {
        H5.H5Tequal(-1, -1);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Tlock_invalid() throws Throwable {
        H5.H5Tlock(-1);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Tget_class_invalid() throws Throwable {
        H5.H5Tget_class(-1);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Tget_size_invalid() throws Throwable {
        H5.H5Tget_size(-1);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Tset_size_invalid() throws Throwable {
        H5.H5Tset_size(-1, 0);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Tget_order_invalid() throws Throwable {
        H5.H5Tget_order(-1);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Tset_order_invalid() throws Throwable {
        H5.H5Tset_order(-1, 0);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Tget_precision_invalid() throws Throwable {
        H5.H5Tget_precision(-1);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Tget_precision_long_invalid() throws Throwable {
        H5.H5Tget_precision_long(-1);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Tset_precision_invalid() throws Throwable {
        H5.H5Tset_precision(-1, 0);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Tget_offset_invalid() throws Throwable {
        H5.H5Tget_offset(-1);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Tset_offset_invalid() throws Throwable {
        H5.H5Tset_offset(-1, 0);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Tcreate_invalid() throws Throwable {
        H5.H5Tcreate(-1, (long)0);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Topen_null() throws Throwable {
        H5.H5Topen(-1, null, 0);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Topen_invalid() throws Throwable {
        H5.H5Topen(-1, "Bogus", 0);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Tcommit_null() throws Throwable {
        H5.H5Tcommit(-1, null, 0, -1, -1, -1);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Tcommit_invalid() throws Throwable {
        H5.H5Tcommit(-1, "Bogus", -1, -1, -1, -1);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Tget_pad_null() throws Throwable {
        H5.H5Tget_pad(-1, null);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Tget_pad_invalid() throws Throwable {
        int[] pad = new int[2];
        H5.H5Tget_pad(-1, pad);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Tset_pad_invalid() throws Throwable {
        H5.H5Tset_pad(-1, -1, -1);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Tget_sign_invalid() throws Throwable {
        H5.H5Tget_sign(-1);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Tset_sign_invalid() throws Throwable {
        H5.H5Tset_sign(-1, 0);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Tget_fields_null() throws Throwable {
        H5.H5Tget_fields(-1, (long[])null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testH5Tget_fields_length_invalid() throws Throwable {
        long[] fields = new long[2];
        H5.H5Tget_fields(-1, fields);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Tget_fields_invalid() throws Throwable {
        long[] fields = new long[5];
        H5.H5Tget_fields(-1, fields);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Tset_fields_invalid() throws Throwable {
        H5.H5Tset_fields(-1, -1, -1, -1, -1, -1);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Tget_ebias_invalid() throws Throwable {
        H5.H5Tget_ebias(-1);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Tget_ebias_long_invalid() throws Throwable {
        H5.H5Tget_ebias_long(-1);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Tset_ebias_invalid() throws Throwable {
        H5.H5Tset_ebias(-1, 0);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Tget_norm_invalid() throws Throwable {
        H5.H5Tget_norm(-1);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Tset_norm_invalid() throws Throwable {
        H5.H5Tset_norm(-1, 0);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Tget_inpad_invalid() throws Throwable {
        H5.H5Tget_inpad(-1);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Tset_inpad_invalid() throws Throwable {
        H5.H5Tset_inpad(-1, 0);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Tget_cset_invalid() throws Throwable {
        H5.H5Tget_cset(-1);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Tset_cset_invalid() throws Throwable {
        H5.H5Tset_cset(-1, 0);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Tget_strpad_invalid() throws Throwable {
        H5.H5Tget_strpad(-1);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Tset_strpad_invalid() throws Throwable {
        H5.H5Tset_strpad(-1, 0);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Tget_nmembers_invalid() throws Throwable {
        H5.H5Tget_nmembers(-1);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Tget_member_index_null() throws Throwable {
        H5.H5Tget_member_index(-1, null);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Tget_member_index_invalid() throws Throwable {
        H5.H5Tget_member_index(-1, "Bogus");
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Tget_member_type_invalid() throws Throwable {
        H5.H5Tget_member_type(-1, -1);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Tget_member_class_invalid() throws Throwable {
        H5.H5Tget_member_class(-1, -1);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Tinsert_null() throws Throwable {
        H5.H5Tinsert(-1, null, 0, 0);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Tinsert_invalid() throws Throwable {
        H5.H5Tinsert(-1, "Bogus", 0, 0);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Tpack_invalid() throws Throwable {
        H5.H5Tpack(-1);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Tvlen_create_invalid() throws Throwable {
        H5.H5Tvlen_create(-1);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Tset_tag_null() throws Throwable {
        H5.H5Tset_tag(-1, null);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Tset_tag_invalid() throws Throwable {
        H5.H5Tset_tag(-1, "Bogus");
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Tget_super_invalid() throws Throwable {
        H5.H5Tget_super(-1);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Tenum_create_invalid() throws Throwable {
        H5.H5Tenum_create(-1);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Tenum_insert_name_null() throws Throwable {
        H5.H5Tenum_insert(-1, null, (byte[])null);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Tenum_insert_null() throws Throwable {
        H5.H5Tenum_insert(-1, "bogus", (byte[])null);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Tenum_insert_invalid() throws Throwable {
        byte[] enumtype = new byte[2];
        H5.H5Tenum_insert(-1, "bogus", enumtype);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testH5Tenum_nameof_invalid_size() throws Throwable {
        H5.H5Tenum_nameof(-1, null, -1);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Tenum_nameof_value_null() throws Throwable {
        H5.H5Tenum_nameof(-1, null, 1);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Tenum_nameof_invalid() throws Throwable {
        byte[] btype = new byte[2];
        H5.H5Tenum_nameof(-1, btype, 1);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Tenum_valueof_name_null() throws Throwable {
        H5.H5Tenum_valueof(-1, null, (byte[])null);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Tenum_valueof_null() throws Throwable {
        H5.H5Tenum_valueof(-1, "bogus", (byte[])null);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Tenum_valueof_invalid() throws Throwable {
        byte[] btype = new byte[2];
        H5.H5Tenum_valueof(-1, "bogus", btype);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Tget_member_value_null() throws Throwable {
        H5.H5Tget_member_value(-1, -1, (byte[])null);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Tget_member_value_invalid() throws Throwable {
        byte[] btype = new byte[2];
        H5.H5Tget_member_value(-1, -1, btype);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testH5Tarray_create_invalid() throws Throwable {
        H5.H5Tarray_create(-1, -1, null);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Tarray_create_value_null() throws Throwable {
        H5.H5Tarray_create(-1, 1, null);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Tget_array_ndims_invalid() throws Throwable {
        H5.H5Tget_array_ndims(-1);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Tget_array_dims_null() throws Throwable {
        H5.H5Tget_array_dims(-1, null);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Tget_native_type_invalid() throws Throwable {
        H5.H5Tget_native_type(-1);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Tflush_invalid() throws Throwable {
        H5.H5Tflush(-1);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Trefresh_invalid() throws Throwable {
        H5.H5Trefresh(-1);
    }

}
