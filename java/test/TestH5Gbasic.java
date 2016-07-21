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

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;

import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;
import hdf.hdf5lib.exceptions.HDF5LibraryException;
import hdf.hdf5lib.structs.H5G_info_t;

import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

public class TestH5Gbasic {
    @Rule public TestName testname = new TestName();
    private static final String H5_FILE = "test.h5";
    long H5fid = -1;

    private final long _createGroup(long fid, String name) {
        long gid = -1;
        try {
            gid = H5.H5Gcreate(fid, name, HDF5Constants.H5P_DEFAULT,
                        HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Gcreate: " + err);
        }

        return gid;
    }

    private final void _deleteFile(String filename) {
        File file = new File(filename);

        if (file.exists()) {
            try {file.delete();} catch (SecurityException e) {}
        }
    }

    @Before
    public void createH5file()
            throws HDF5LibraryException, NullPointerException {
        assertTrue("H5 open ids is 0",H5.getOpenIDCount()==0);
        System.out.print(testname.getMethodName());

        H5fid = H5.H5Fcreate(H5_FILE, HDF5Constants.H5F_ACC_TRUNC,
                HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        H5.H5Fflush(H5fid, HDF5Constants.H5F_SCOPE_LOCAL);
    }

    @After
    public void deleteH5file() throws HDF5LibraryException {
        if (H5fid > 0) {
            try {H5.H5Fclose(H5fid);} catch (Exception ex) {}
        }
        _deleteFile(H5_FILE);
        System.out.println();
    }

    @Test//(expected = HDF5LibraryException.class)
    public void testH5Gclose_invalid() throws Throwable {
        long gid = H5.H5Gclose(-1);
        assertTrue(gid == 0);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Gcreate_null() throws Throwable {
        long gid = -1;

        // it should fail because the group name is null
        gid = H5.H5Gcreate(H5fid, null, HDF5Constants.H5P_DEFAULT,
                    HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);

        try {H5.H5Gclose(gid);} catch (Exception ex) {}
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Gcreate_invalid() throws Throwable {
        H5.H5Gcreate(-1, "Invalid ID", HDF5Constants.H5P_DEFAULT,
                    HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
    }

    @Test
    public void testH5Gcreate() {
        long gid = -1;
        try {
            gid = H5.H5Gcreate(H5fid, "/testH5Gcreate",
                        HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT,
                        HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Gcreate: " + err);
        }
        assertTrue(gid > 0);

        try {H5.H5Gclose(gid);} catch (Exception ex) {}
    }

    @Test
    public void testH5Gclose() {
        long gid = _createGroup(H5fid, "/testH5Gcreate");
        assertTrue(gid > 0);

        try {
            H5.H5Gclose(gid);
        }
        catch (Throwable err) {
            fail("H5Gclose: " + err);
        }
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Gcreate_exists() throws Throwable {
        long gid = _createGroup(H5fid, "/testH5Gcreate");
        assertTrue(gid > 0);

        try {H5.H5Gclose(gid);} catch (Exception ex) {}

        // it should failed now because the group already exists in file
        gid = H5.H5Gcreate(H5fid, "/testH5Gcreate",
                    HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT,
                    HDF5Constants.H5P_DEFAULT);
    }

    @Test
    public void testH5Gcreate_anon() {
        long gid = -1;
        try {
            gid = H5.H5Gcreate_anon(H5fid, HDF5Constants.H5P_DEFAULT,
                    HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Gcreate_anon: " + err);
        }
        assertTrue(gid > 0);

        try {H5.H5Gclose(gid);} catch (Exception ex) {}
    }

    @Test(expected = NullPointerException.class)
    public void testH5Gopen_null() throws Throwable {
        long gid = -1;

        gid = H5.H5Gopen(H5fid, null, HDF5Constants.H5P_DEFAULT);

        try {H5.H5Gclose(gid);} catch (Exception ex) {}
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Gopen_invalid() throws Throwable {
        H5.H5Gopen(-1, "Invalid ID", HDF5Constants.H5P_DEFAULT);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Gopen_not_exists() throws Throwable {
        long gid = -1;

         gid = H5.H5Gopen(H5fid, "Never_created", HDF5Constants.H5P_DEFAULT);

        try {H5.H5Gclose(gid);} catch (Exception ex) {}
    }

    @Test
    public void testH5Gopen() {
        long gid = _createGroup(H5fid, "/testH5Gcreate");
        assertTrue(gid > 0);

        try {H5.H5Gclose(gid);} catch (Exception ex) {}

        try {
            gid = H5.H5Gopen(H5fid, "/testH5Gcreate",
                        HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Gopen: " + err);
        }
        assertTrue(gid > 0);

        try {H5.H5Gclose(gid);} catch (Exception ex) {}
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Gget_create_plist_invalid() throws Throwable {
        H5.H5Gget_create_plist(-1);
    }

    @Test
    public void testH5Gget_create_plist() {
        long pid = -1;
        long gid = _createGroup(H5fid, "/testH5Gcreate");
        assertTrue(gid > 0);

        try {
            pid = H5.H5Gget_create_plist(gid);
        }
        catch (Throwable err) {
            try {H5.H5Gclose(gid);} catch (Exception ex) {}
            err.printStackTrace();
            fail("H5.H5Gget_create_plist: " + err);
        }
        assertTrue(pid > 0);

        try {H5.H5Pclose(pid);} catch (Exception ex) {}

        try {H5.H5Gclose(gid);} catch (Exception ex) {}
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Gget_info_invalid() throws Throwable {
        H5.H5Gget_info(-1);
    }

    @Test
    public void testH5Gget_info() {
        H5G_info_t info = null;
        long gid = _createGroup(H5fid, "/testH5Gcreate");
        assertTrue(gid > 0);

        try {
            info = H5.H5Gget_info(gid);
        }
        catch (Throwable err) {
            try {H5.H5Gclose(gid);} catch (Exception ex) {}
            err.printStackTrace();
            fail("H5.H5Gget_info: " + err);
        }
        assertNotNull(info);

        try {H5.H5Gclose(gid);} catch (Exception ex) {}
    }

    @Test(expected = NullPointerException.class)
    public void testH5Gget_info_by_name_null() throws Throwable {
        H5.H5Gget_info_by_name(-1, null, HDF5Constants.H5P_DEFAULT);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Gget_info_by_name_invalid() throws Throwable {
        H5.H5Gget_info_by_name(-1, "/testH5Gcreate", HDF5Constants.H5P_DEFAULT);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Gget_info_by_name_not_exists() throws Throwable {
        H5.H5Gget_info_by_name(H5fid, "/testH5Gcreate",
                HDF5Constants.H5P_DEFAULT);
    }

    @Test
    public void testH5Gget_info_by_name() {
        H5G_info_t info = null;
        long gid = _createGroup(H5fid, "/testH5Gcreate");
        assertTrue(gid > 0);

        try {
            info = H5.H5Gget_info_by_name(gid, "/testH5Gcreate",
                    HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            try {H5.H5Gclose(gid);} catch (Exception ex) {}
            err.printStackTrace();
            fail("H5.H5Gget_info_by_name: " + err);
        }
        assertNotNull(info);

        try {H5.H5Gclose(gid);} catch (Exception ex) {}
    }

    @Test
    public void testH5Gget_info_by_name_fileid() {
        H5G_info_t info = null;
        long gid = _createGroup(H5fid, "/testH5Gcreate");
        assertTrue(gid > 0);
        try {H5.H5Gclose(gid);} catch (Exception ex) {}

        try {
            info = H5.H5Gget_info_by_name(H5fid, "/testH5Gcreate",
                    HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            try {H5.H5Gclose(gid);} catch (Exception ex) {}
            err.printStackTrace();
            fail("H5.H5Gget_info_by_name: " + err);
        }
        assertNotNull(info);

        try {H5.H5Gclose(gid);} catch (Exception ex) {}
    }

    @Test(expected = NullPointerException.class)
    public void testH5Gget_info_by_idx_null() throws Throwable {
        H5.H5Gget_info_by_idx(-1, null, HDF5Constants.H5_INDEX_NAME,
                HDF5Constants.H5_ITER_INC, 1L, HDF5Constants.H5P_DEFAULT);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Gget_info_by_idx_invalid() throws Throwable {
        H5.H5Gget_info_by_idx(-1, "/testH5Gcreate", HDF5Constants.H5_INDEX_NAME,
                HDF5Constants.H5_ITER_INC, 1L, HDF5Constants.H5P_DEFAULT);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Gget_info_by_idx_not_exists() throws Throwable {
        H5.H5Gget_info_by_idx(H5fid, "/testH5Gcreate",
                HDF5Constants.H5_INDEX_NAME, HDF5Constants.H5_ITER_INC, 1L,
                HDF5Constants.H5P_DEFAULT);
    }

    @Test
    public void testH5Gget_info_by_idx() {
        H5G_info_t info = null;
        long gid = _createGroup(H5fid, "/testH5Gcreate");
        assertTrue(gid > 0);

        try {
            info = H5.H5Gget_info_by_idx(gid, "/", HDF5Constants.H5_INDEX_NAME,
                    HDF5Constants.H5_ITER_INC, 0, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Gget_info_by_idx: " + err);
        }
        assertNotNull(info);

        try {H5.H5Gclose(gid);} catch (Exception ex) {}
    }

    @Test
    public void testH5Gget_info_by_idx_fileid() {
        H5G_info_t info = null;
        long gid = _createGroup(H5fid, "/testH5Gcreate");
        assertTrue(gid > 0);
        try {H5.H5Gclose(gid);} catch (Exception ex) {}

        try {
            info = H5.H5Gget_info_by_idx(H5fid, "/",
                    HDF5Constants.H5_INDEX_NAME, HDF5Constants.H5_ITER_INC, 0,
                    HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Gget_info_by_idx: " + err);
        }
        assertNotNull(info);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Gflush_invalid() throws Throwable {
        H5.H5Gflush(-1);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Grefresh_invalid() throws Throwable {
        H5.H5Grefresh(-1);
    }

}
