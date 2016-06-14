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

public class TestH5Dparams {
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
    public void testH5Dclose_invalid() throws Throwable {
        long did = H5.H5Dclose(-1);
        assertTrue(did == 0);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Dcreate_null() throws Throwable {
        H5.H5Dcreate(-1, null, 0, 0, 0, 0, 0);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Dcreate_invalid() throws Throwable {
        H5.H5Dcreate(-1, "Bogus", -1, -1, -1, -1, -1);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Dcreate_anon_invalid() throws Throwable {
        H5.H5Dcreate_anon(-1, -1, -1, -1, -1);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Dget_access_plist_invalid() throws Throwable {
        H5.H5Dget_access_plist(-1);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Dget_create_plist_invalid() throws Throwable {
        H5.H5Dget_create_plist(-1);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Dget_offset_invalid() throws Throwable {
        H5.H5Dget_offset(-1);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Dget_space_invalid() throws Throwable {
        H5.H5Dget_space(-1);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Dget_type_invalid() throws Throwable {
        H5.H5Dget_type(-1);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Dget_space_status_invalid() throws Throwable {
        int status = H5.H5Dget_space_status(-1);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Dset_extent_status_invalid() throws Throwable {
        long[] size = new long[2];
        H5.H5Dset_extent(-1, size);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Dset_extent_status_null() throws Throwable {
        H5.H5Dset_extent(-1, null);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Dopen_null() throws Throwable {
        H5.H5Dopen(-1, null, 0);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Dopen_invalid() throws Throwable {
        H5.H5Dopen(-1, "Bogus", 0);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Dvlen_get_buf_size_invalid() throws Throwable {
        H5.H5Dvlen_get_buf_size(-1, -1, -1);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Dvlen_reclaim_invalid() throws Throwable {
        byte[] buf = new byte[2];
        H5.H5Dvlen_reclaim(-1, -1, -1, buf);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Dvlen_reclaim_null() throws Throwable {
        H5.H5Dvlen_reclaim(-1, -1, -1, null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testH5Dget_storage_size_invalid() throws Throwable {
        H5.H5Dget_storage_size(-1);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Dflush_invalid() throws Throwable {
        H5.H5Dflush(-1);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Drefresh_invalid() throws Throwable {
        H5.H5Drefresh(-1);
    }

}
