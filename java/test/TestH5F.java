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
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;

import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;
import hdf.hdf5lib.exceptions.HDF5LibraryException;

import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

public class TestH5F {
    @Rule public TestName testname = new TestName();
    private static final String H5_FILE = "test.h5";

    private static final int COUNT_OBJ_FILE = 1;
    private static final int COUNT_OBJ_DATASET = 0;
    private static final int COUNT_OBJ_GROUP = 0;
    private static final int COUNT_OBJ_DATATYPE = 0;
    private static final int COUNT_OBJ_ATTR = 0;
    private static final int COUNT_OBJ_ALL = (COUNT_OBJ_FILE
            + COUNT_OBJ_DATASET + COUNT_OBJ_GROUP + COUNT_OBJ_DATATYPE + COUNT_OBJ_ATTR);
    private static final int[] OBJ_COUNTS = { COUNT_OBJ_FILE,
            COUNT_OBJ_DATASET, COUNT_OBJ_GROUP, COUNT_OBJ_DATATYPE,
            COUNT_OBJ_ATTR, COUNT_OBJ_ALL };
    private static final int[] OBJ_TYPES = { HDF5Constants.H5F_OBJ_FILE,
            HDF5Constants.H5F_OBJ_DATASET, HDF5Constants.H5F_OBJ_GROUP,
            HDF5Constants.H5F_OBJ_DATATYPE, HDF5Constants.H5F_OBJ_ATTR,
            HDF5Constants.H5F_OBJ_ALL };
    long H5fid = -1;

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
            H5fid = -1;
        }
        _deleteFile(H5_FILE);
        System.out.println();
    }

    @Test
    public void testH5Fget_create_plist() {
        long plist = -1;

        try {
            plist = H5.H5Fget_create_plist(H5fid);
        }
        catch (Throwable err) {
            fail("H5.H5Fget_create_plist: " + err);
        }
        assertTrue(plist > 0);
        try {H5.H5Pclose(plist);} catch (HDF5LibraryException e) {e.printStackTrace();}
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Fget_create_plist_closed() throws Throwable {
        long fid = -1;

        if (H5fid > 0) {
            try {H5.H5Fclose(H5fid);} catch (Exception ex) {}
            H5fid = -1;
        }

        try {
            fid = H5.H5Fopen(H5_FILE, HDF5Constants.H5F_ACC_RDWR,
                    HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            fail("H5.H5Fopen: " + err);
        }
        try {
            H5.H5Fclose(fid);
        }
        catch (Exception ex) {
        }

        // it should fail because the file was closed.
        H5.H5Fget_create_plist(fid);
    }

    @Test
    public void testH5Fget_access_plist() {
        long plist = -1;

        try {
            plist = H5.H5Fget_access_plist(H5fid);
        }
        catch (Throwable err) {
            fail("H5.H5Fget_access_plist: " + err);
        }
        assertTrue(plist > 0);
        try {H5.H5Pclose(plist);} catch (HDF5LibraryException e) {e.printStackTrace();}
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Fget_access_plist_closed() throws Throwable {
        long fid = -1;

        if (H5fid > 0) {
            try {H5.H5Fclose(H5fid);} catch (Exception ex) {}
            H5fid = -1;
        }

        try {
            fid = H5.H5Fopen(H5_FILE, HDF5Constants.H5F_ACC_RDWR,
                    HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            fail("H5.H5Fopen: " + err);
        }
        try {
            H5.H5Fclose(fid);
        }
        catch (Exception ex) {
        }

        // it should fail because the file was closed.
        H5.H5Fget_access_plist(fid);
    }

    @Test
    public void testH5Fget_intent_rdwr() {
        int intent = 0;
        long fid = -1;

        if (H5fid > 0) {
            try {H5.H5Fclose(H5fid);} catch (Exception ex) {}
            H5fid = -1;
        }

        try {
            fid = H5.H5Fopen(H5_FILE, HDF5Constants.H5F_ACC_RDWR,
                    HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            fail("H5.H5Fopen: " + err);
        }
        try {
            intent = H5.H5Fget_intent(fid);
        }
        catch (Throwable err) {
            fail("H5.H5Fget_intent: " + err);
        }
        assertEquals(HDF5Constants.H5F_ACC_RDWR, intent);

        try {
            H5.H5Fclose(fid);
        }
        catch (Exception ex) {
        }
    }

    @Test
    public void testH5Fget_intent_rdonly() {
        int intent = 0;
        long fid = -1;

        if (H5fid > 0) {
            try {H5.H5Fclose(H5fid);} catch (Exception ex) {}
            H5fid = -1;
        }

        try {
            fid = H5.H5Fopen(H5_FILE, HDF5Constants.H5F_ACC_RDONLY,
                    HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            fail("H5.H5Fopen: " + err);
        }
        try {
            intent = H5.H5Fget_intent(fid);
        }
        catch (Throwable err) {
            fail("H5.H5Fget_intent: " + err);
        }
        assertEquals(HDF5Constants.H5F_ACC_RDONLY, intent);

        try {
            H5.H5Fclose(fid);
        }
        catch (Exception ex) {
        }
    }

    @Test
    public void testH5Fget_obj_count() {
        long count = -1;

        for (int i = 0; i < OBJ_TYPES.length; i++) {
            try {
                count = H5.H5Fget_obj_count(H5fid, OBJ_TYPES[i]);
            }
            catch (Throwable err) {
                fail("H5.H5Fget_obj_count: " + err);
            }

            assertEquals(count, OBJ_COUNTS[i]);
        }
    }

    @Test
    public void testH5Fget_obj_ids() {
        long count = 0;
        long max_objs = 100;
        long[] obj_id_list = new long[(int)max_objs];
        int[] open_obj_counts = new int[OBJ_TYPES.length];

        for (int i = 0; i < OBJ_TYPES.length; i++)
            open_obj_counts[i] = 0;

        open_obj_counts[0] = 1;
        for (int i = 0; i < OBJ_TYPES.length - 1; i++)
            open_obj_counts[OBJ_TYPES.length - 1] += open_obj_counts[i];

        for (int i = 0; i < OBJ_TYPES.length; i++) {
            try {
                count = H5.H5Fget_obj_ids(H5fid, OBJ_TYPES[i], max_objs,
                        obj_id_list);
            }
            catch (Throwable err) {
                fail("H5.H5Fget_obj_ids: " + err);
            }
            assertEquals(count, open_obj_counts[i]);
        }
    }
    /**
     * TODO: need to add objects to the file after H5G[D,T]create() functions
     * are implemented.
     */
    //
    // @Test
    // public void testH5Fmount_unmount() {
    // String file1 = "src.h5";
    // String file2 = "dst.h5";
    // String group1 = "/G";
    // String group2 = "/MOUNTED";
    //
    // _createH5File(file1);
    // _createH5File(file2);
    //
    // int fid1 = _openFile(file1, HDF5Constants.H5F_ACC_RDWR);
    // assertTrue(fid1 > 0);
    //
    // int fid2 = _openFile(file2, HDF5Constants.H5F_ACC_RDWR);
    // assertTrue(fid2 > 0);
    //
    // // create a group at file1
    // int gid = _createGroup(fid1, group1);
    // try { H5.H5Gclose(gid); } catch (Exception ex) {}
    //
    // // create a group at file 2
    // gid = _createGroup(fid2, group2);
    // try { H5.H5Gclose(gid); } catch (Exception ex) {}
    //
    // // before mount, "/G/MOUNTED" does not exists in file1
    // gid = _openGroup(fid1, group1+group2);
    // assertTrue(gid < 0);
    //
    // // Mount file2 under G in the file1
    // try {
    // H5.H5Fmount(fid1, group1, fid2, HDF5Constants.H5P_DEFAULT);
    // }
    // catch (Throwable err) {
    // fail("H5.H5Fmount: "+err);
    // }
    //
    // // now file1 should have group "/G/MOUNTED"
    // gid = _openGroup(fid1, group1+group2);
    // assertTrue(gid > 0);
    // try { H5.H5Gclose(gid); } catch (Exception ex) {}
    //
    // // unmount file2 from file1
    // try {
    // H5.H5Funmount(fid1, group1);
    // }
    // catch (Throwable err) {
    // fail("H5.H5Funmount: "+err);
    // }
    //
    // // file2 was unmounted from file1, "/G/MOUNTED" does not exists in file1
    // gid = _openGroup(fid1, group1+group2);
    // assertTrue(gid < 0);
    //
    // try { H5.H5Fclose(fid1); } catch (Exception ex) {}
    // try { H5.H5Fclose(fid2); } catch (Exception ex) {}
    //
    // _deleteFile(file1);
    // _deleteFile(file2);
    // }
}
