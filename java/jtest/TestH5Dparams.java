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

public class TestH5Dparams {
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
    public void testH5Dclose_invalid() throws Throwable
    {
        long did = H5Dclose(-1);
        assertTrue(did == 0);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Dcreate_null() throws Throwable
    {
        H5Dcreate(-1, null, 0, 0, 0, 0, 0);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Dcreate_invalid() throws Throwable
    {
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment name_segment = arena.allocateFrom("Bogus");
            H5Dcreate2(-1, name_segment, -1, -1, -1, -1, -1);
        }
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Dcreate_anon_invalid() throws Throwable
    {
        H5Dcreate_anon(-1, -1, -1, -1, -1);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Dget_access_plist_invalid() throws Throwable
    {
        H5Dget_access_plist(-1);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Dget_create_plist_invalid() throws Throwable
    {
        H5Dget_create_plist(-1);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Dget_offset_invalid() throws Throwable
    {
        H5Dget_offset(-1);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Dget_space_invalid() throws Throwable
    {
        H5Dget_space(-1);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Dget_type_invalid() throws Throwable
    {
        H5Dget_type(-1);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Dget_space_status_invalid() throws Throwable
    {
        int status = H5Dget_space_status(-1);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Dset_extent_status_invalid() throws Throwable
    {
        long[] size = new long[2];
        H5Dset_extent(-1, size);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Dset_extent_status_null() throws Throwable
    {
        H5Dset_extent(-1, null);
    }

    public void testH5Dopen_null() throws Throwable
    {
        long did = H5Dopen(-1, null, 0);
        assertTrue("H5Dopen", did < 0);
    }

    public void testH5Dopen_invalid() throws Throwable
    {
        long did = H5I_INVALID_HID();
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment filename_segment = arena.allocateFrom("Bogus");
            did                            = H5Dopen(-1, filename_segment, 0);
        }
        assertTrue("H5Dopen", did < 0);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Dvlen_get_buf_size_invalid() throws Throwable
    {
        H5Dvlen_get_buf_size(-1, -1, -1);
    }

    @Test(expected = HDF5FunctionArgumentException.class)
    public void testH5Dget_storage_size_invalid() throws Throwable
    {
        H5Dget_storage_size(-1);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Dflush_invalid() throws Throwable
    {
        H5Dflush(-1);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Drefresh_invalid() throws Throwable
    {
        H5Drefresh(-1);
    }
}
