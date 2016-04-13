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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;

import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;
import hdf.hdf5lib.structs.H5F_info2_t;

import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

public class TestH5Fparams {
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

    @Test(expected = NullPointerException.class)
    public void testH5Fcreate_null() throws Throwable {
        H5.H5Fcreate(null, HDF5Constants.H5F_ACC_TRUNC,
                HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Fopen_null() throws Throwable {
        H5.H5Fopen(null, HDF5Constants.H5F_ACC_RDWR, HDF5Constants.H5P_DEFAULT);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Fis_hdf5_null() throws Throwable {
        H5.H5Fis_hdf5(null);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Fmount_null() throws Throwable {
        H5.H5Fmount(-1, null, -1, HDF5Constants.H5P_DEFAULT);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Funmount_null() throws Throwable {
        H5.H5Funmount(-1, null);
    }

    @Test
    public void testH5Fis_hdf5_text() {
        File txtFile = null;
        boolean isH5 = false;

        try {
            txtFile = new File("test.txt");
            if (!txtFile.exists())
                txtFile.createNewFile();
            isH5 = H5.H5Fis_hdf5("test.txt");
        }
        catch (Throwable err) {
            fail("H5.H5Fis_hdf5 failed on test.txt: " + err);
        }

        assertFalse(isH5);

        try {
            txtFile.delete();
        }
        catch (SecurityException e) {
            ;// e.printStackTrace();
        }
    }

    @Test//(expected = HDF5LibraryException.class)
    public void testH5Fclose_negative() throws Throwable {
        // cannot close a file with negative id.
        int fid = H5.H5Fclose(-1);
        assertTrue(fid == 0);
    }

    @Test
    public void testH5Fcreate() {
        long fid = -1;
        File file = null;

        try {
            fid = H5.H5Fcreate("test.h5", HDF5Constants.H5F_ACC_TRUNC,
                HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
            if (fid > 0) {
                H5.H5Fclose(fid);
            }
            file = new File("test.h5");
        }
        catch (Throwable err) {
            fail("H5.H5Fopen: " + err);
        }

        if (file.exists()) {
            try {
                file.delete();
            }
            catch (SecurityException e) {
                ;// e.printStackTrace();
            }
        }
    }

    @Test
    public void testH5Fflush_global() {
        long fid = -1;

        try {
            fid = H5.H5Fcreate("test.h5", HDF5Constants.H5F_ACC_TRUNC,
                    HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            fail("H5.H5Fopen: " + err);
        }

        try {
            H5.H5Fflush(fid, HDF5Constants.H5F_SCOPE_GLOBAL);
        }
        catch (Throwable err) {
            fail("H5.H5Fflush: " + err);
        }

        try {
            H5.H5Fclose(fid);
        }
        catch (Exception ex) {
        }
    }

    @Test
    public void testH5Fflush_local() {
        long fid = -1;

        try {
            fid = H5.H5Fcreate("test.h5", HDF5Constants.H5F_ACC_TRUNC,
                    HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            fail("H5.H5Fopen: " + err);
        }

        try {
            H5.H5Fflush(fid, HDF5Constants.H5F_SCOPE_LOCAL);
        }
        catch (Throwable err) {
            fail("H5.H5Fflush: " + err);
        }

        try {
            H5.H5Fclose(fid);
        }
        catch (Exception ex) {
        }
    }

    @Test
    public void testH5Fget_info() {
        long fid = -1;

        try {
            try {
                fid = H5.H5Fcreate("test.h5", HDF5Constants.H5F_ACC_TRUNC,
                        HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
            }
            catch (Throwable err) {
                fail("H5.H5Fopen: " + err);
            }

            try {
                H5F_info2_t finfo = H5.H5Fget_info(fid);
                assertEquals(finfo.super_version, 0);
                assertEquals(finfo.free_version, 0);
                assertEquals(finfo.sohm_version, 0);
            }
            catch (Throwable err) {
                fail("H5.H5Fget_info: " + err);
            }
        }
        catch (Exception e) {
           e.printStackTrace();
        }
        finally {
            try {H5.H5Fclose(fid);} catch (Exception ex) {}
        }
    }
}
