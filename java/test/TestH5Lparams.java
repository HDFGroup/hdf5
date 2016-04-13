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

public class TestH5Lparams {
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

    @Test(expected = HDF5LibraryException.class)
    public void testH5Lget_value_invalid() throws Throwable {
        H5.H5Lget_value(-1, "Bogus", null, -1);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Lget_value_null() throws Throwable {
        H5.H5Lget_value(-1, null, null, 0);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Lexists_invalid() throws Throwable {
        H5.H5Lexists(-1, "Bogus", -1);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Lexists_null() throws Throwable {
        H5.H5Lexists(-1, null, 0);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Lget_info_invalid() throws Throwable {
        H5.H5Lget_info(-1, "Bogus", -1);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Lget_info_null() throws Throwable {
        H5.H5Lget_info(-1, null, 0);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Lget_info_by_idx_invalid() throws Throwable {
        H5.H5Lget_info_by_idx(-1, "Bogus", -1, -1, -1L, -1);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Lget_info_by_idx_null() throws Throwable {
        H5.H5Lget_info_by_idx(-1, null, 0, 0, 0L, 0);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Lget_name_by_idx_invalid() throws Throwable {
        H5.H5Lget_name_by_idx(-1, "Bogus", -1, -1, -1L, -1);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Lget_name_by_idx_null() throws Throwable {
        H5.H5Lget_name_by_idx(-1, null, 0, 0, 0L, 0);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Lcreate_hard_invalid() throws Throwable {
        H5.H5Lcreate_hard(-1, "Bogus", -1, "Bogus", -1, -1);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Lcreate_hard_null_current() throws Throwable {
        H5.H5Lcreate_hard(-1, null, 0, "Bogus", 0, 0);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Lcreate_hard_null_dest() throws Throwable {
        H5.H5Lcreate_hard(-1, "Bogus", 0, null, 0, 0);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Ldelete_invalid() throws Throwable {
        H5.H5Ldelete(-1, "Bogus", -1);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Ldelete_null() throws Throwable {
        H5.H5Ldelete(-1, null, 0);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Lcreate_soft_invalid() throws Throwable {
        H5.H5Lcreate_soft( "Bogus", -1, "Bogus", -1, -1);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Lcreate_soft_null_current() throws Throwable {
        H5.H5Lcreate_soft(null, 0, "Bogus", 0, 0);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Lcreate_soft_null_dest() throws Throwable {
        H5.H5Lcreate_soft("Bogus", 0, null, 0, 0);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Lcreate_external_invalid() throws Throwable {
        H5.H5Lcreate_external("PathToFile", "Bogus", -1, "Bogus", -1, -1);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Lcreate_external_null_file() throws Throwable {
        H5.H5Lcreate_external(null, "Bogus", 0, "Bogus", 0, 0);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Lcreate_external_null_current() throws Throwable {
        H5.H5Lcreate_external("PathToFile", null, 0, "Bogus", 0, 0);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Lcreate_external_null_dest() throws Throwable {
        H5.H5Lcreate_external("PathToFile", "Bogus", 0, null, 0, 0);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Lcopy_invalid() throws Throwable {
        H5.H5Lcopy(-1, "Bogus", -1, "Bogus", -1, -1);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Lcopy_null_current() throws Throwable {
        H5.H5Lcopy(-1, null, 0, "Bogus", 0, 0);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Lcopy_null_dest() throws Throwable {
        H5.H5Lcopy(-1, "Bogus", 0, null, 0, 0);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Lmove_invalid() throws Throwable {
        H5.H5Lmove(-1, "Bogus", -1, "Bogus", -1, -1);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Lmove_null_current() throws Throwable {
        H5.H5Lmove(-1, null, 0, "Bogus", 0, 0);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Lmove_null_dest() throws Throwable {
        H5.H5Lmove(-1, "Bogus", 0, null, 0, 0);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Lget_value_by_idx_invalid() throws Throwable {
        H5.H5Lget_value_by_idx(-1, "Bogus", -1, -1, -1L, null, -1);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Lget_value_by_idx_null() throws Throwable {
        H5.H5Lget_value_by_idx(-1, null, 0, 0, 0L, null, 0);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Ldelete_by_idx_invalid() throws Throwable {
        H5.H5Ldelete_by_idx(-1, "Bogus", -1, -1, -1L, -1);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Ldelete_by_idx_null() throws Throwable {
        H5.H5Ldelete_by_idx(-1, null, 0, 0, 0L, 0);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Lvisit_null() throws Throwable {
        H5.H5Lvisit(-1, -1, -1, null, null);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Lvisit_by_name_nullname() throws Throwable {
        H5.H5Lvisit_by_name(-1, null, -1, -1, null, null, -1);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Lvisit_by_name_null() throws Throwable {
        H5.H5Lvisit_by_name(-1, "Bogus", -1, -1, null, null, -1);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Literate_null() throws Throwable {
        H5.H5Literate(-1, -1, -1, -1, null, null);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Literate_by_name_nullname() throws Throwable {
        H5.H5Literate_by_name(-1, null, -1, -1, -1, null, null, -1);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Literate_by_name_null() throws Throwable {
        H5.H5Literate_by_name(-1, "Bogus", -1, -1, -1, null, null, -1);
    }

}
