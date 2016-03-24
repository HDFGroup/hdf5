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
import hdf.hdf5lib.HDF5Constants;
import hdf.hdf5lib.exceptions.HDF5LibraryException;

import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

public class TestH5Oparams {
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
    public void testH5Oclose_invalid() throws Throwable {
        long oid = H5.H5Oclose(-1);
        assertTrue(oid == 0);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Oopen_null() throws Throwable {
        H5.H5Oopen(-1, null, 0);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Oopen_invalid() throws Throwable {
        H5.H5Oopen(-1, "Bogus", 0);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Ocopy_invalid() throws Throwable {
        H5.H5Ocopy(-1, "Bogus", -1, "Bogus", -1, -1);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Ocopy_null_current() throws Throwable {
        H5.H5Ocopy(-1, null, 0, "Bogus", 0, 0);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Ocopy_null_dest() throws Throwable {
        H5.H5Ocopy(-1, "Bogus", 0, null, 0, 0);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Oget_info_invalid() throws Throwable {
        H5.H5Oget_info(-1);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Oget_info_by_name_null() throws Throwable {
        H5.H5Oget_info_by_name(-1, null, HDF5Constants.H5P_DEFAULT);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Oget_info_by_name_invalid() throws Throwable {
        H5.H5Oget_info_by_name(-1, "/testH5Gcreate", HDF5Constants.H5P_DEFAULT);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Oget_info_by_idx_invalid() throws Throwable {
        H5.H5Oget_info_by_idx(-1, "Bogus", -1, -1, -1L, -1);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Oget_info_by_idx_null() throws Throwable {
        H5.H5Oget_info_by_idx(-1, null, 0, 0, 0L, 0);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Olink_invalid() throws Throwable {
        H5.H5Olink(-1, -1, "Bogus", -1, -1);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Olink_null_dest() throws Throwable {
        H5.H5Olink(-1, 0, null, 0, 0);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Ovisit_null() throws Throwable {
        H5.H5Ovisit(-1, -1, -1, null, null);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Ovisit_by_name_nullname() throws Throwable {
        H5.H5Ovisit_by_name(-1, null, -1, -1, null, null, -1);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Ovisit_by_name_null() throws Throwable {
        H5.H5Ovisit_by_name(-1, "Bogus", -1, -1, null, null, -1);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Oset_comment_invalid() throws Throwable {
        H5.H5Oset_comment(-1, "Bogus");
    }

    @Test(expected = IllegalArgumentException.class)
    public void testH5Oget_comment_invalid() throws Throwable {
        H5.H5Oget_comment(-1);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Oset_comment_by_name_invalid() throws Throwable {
        H5.H5Oset_comment_by_name(-1, "Bogus", null, -1);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Oset_comment_by_name_null() throws Throwable {
        H5.H5Oset_comment_by_name(-1, null, null, -1);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testH5Oget_comment_by_name_invalid() throws Throwable {
        H5.H5Oget_comment_by_name(-1, "Bogus", -1);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Oget_comment_by_name_null() throws Throwable {
        H5.H5Oget_comment_by_name(-1, null, -1);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Oflush_invalid() throws Throwable {
        H5.H5Oflush(-1);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Orefresh_invalid() throws Throwable {
        H5.H5Orefresh(-1);
    }

}
