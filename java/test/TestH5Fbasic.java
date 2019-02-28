/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
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

public class TestH5Fbasic {
    @Rule public TestName testname = new TestName();
    private static final String H5_FILE = "testFb.h5";
    private static final String TXT_FILE = "testFb.txt";
    long H5fid = -1;

    private final void _deleteFile(String filename) {
        File file = new File(filename);

        if (file.exists()) {
            try {file.delete();} catch (SecurityException e) {}
        }
    }

    @Before
    public void createH5file() throws HDF5LibraryException, NullPointerException {
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

    @Test
    public void testH5Fcreate() {
        assertTrue(H5fid > 0);
    }

    @Test
    public void testH5Fis_hdf5() {
        boolean isH5 = false;

        try {
            isH5 = H5.H5Fis_hdf5(H5_FILE);
        }
        catch (Throwable err) {
            fail("H5.H5Fis_hdf5 failed on " + H5_FILE + ": " + err);
        }
        assertTrue(isH5 == true);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Fcreate_EXCL() throws Throwable {
        H5.H5Fcreate(H5_FILE, HDF5Constants.H5F_ACC_EXCL,
                HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Fopen_read_only() throws Throwable {
        long fid = -1;

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

        // set the file to read-only
        File file = new File(H5_FILE);
        if (file.setWritable(false)) {
            // this should fail.
            fid = H5.H5Fopen(H5_FILE, HDF5Constants.H5F_ACC_RDWR,
                    HDF5Constants.H5P_DEFAULT);

            try {
                H5.H5Fclose(fid);
            }
            catch (Exception ex) {
            }
        }
        else {
            fail("File.setWritable(true) failed.");
        }
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Freopen_closed() throws Throwable {
        long fid = -1;
        long fid2 = -1;

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

        // should fail because the file was closed.
        fid2 = H5.H5Freopen(fid);
    }

    @Test
    public void testH5Freopen() {
        long fid = -1;
        long fid2 = -1;

        try {
            fid = H5.H5Fopen(H5_FILE, HDF5Constants.H5F_ACC_RDWR,
                    HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            fail("H5.H5Fopen: " + err);
        }

        try {
            fid2 = H5.H5Freopen(fid);
        }
        catch (Throwable err) {
            fail("H5.H5Freopen: " + err);
        }
        assertTrue(fid2 > 0);

        try {
            H5.H5Fclose(fid2);
        }
        catch (Exception ex) {
        }

        try {
            H5.H5Fclose(fid);
        }
        catch (Exception ex) {
        }
    }

    @Test
    public void testH5Fclose() {
        long fid = -1;

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
        catch (Throwable err) {
            fail("H5.H5Fclose: " + err);
        }
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Fclose_twice() throws Throwable {
        long fid = -1;

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
        catch (Throwable err) {
            fail("H5.H5Fclose: " + err);
        }

        // it should fail since the file was closed.
        H5.H5Fclose(fid);
    }

    @Test
    public void testH5Fget_freespace() {
        long freeSpace = 0;

        try {
            freeSpace = H5.H5Fget_freespace(H5fid);
        }
        catch (Throwable err) {
            fail("H5.H5Fget_freespace: " + err);
        }
        assertEquals(freeSpace, 0);
    }

    // TODO add/and delete objects and test freespace

    @Test
    public void testH5Fget_filesize() {
        long fileSize = 0;

        try {
            fileSize = H5.H5Fget_filesize(H5fid);
        }
        catch (Throwable err) {
            fail("H5.H5Fget_freespace: " + err);
        }
        assertTrue(fileSize > 0);
    }

    // TODO add/and delete objects and test freespace

    @Test
    public void testH5Fget_mdc_hit_rate() {
        double rate;

        try {
            rate = H5.H5Fget_mdc_hit_rate(H5fid);
        }
        catch (Throwable err) {
            fail("H5.H5Fget_mdc_hit_rate: " + err);
        }
    }

    @Test
    public void testH5Fget_mdc_size() {
        int nentries = -1;
        long cache_sizes[] = new long[3];

        try {
            nentries = H5.H5Fget_mdc_size(H5fid, cache_sizes);
        }
        catch (Throwable err) {
            fail("H5.H5Fget_mdc_size: " + err);
        }
        assertTrue("H5.H5Fget_mdc_size #:" + nentries, nentries == 4);
    }

    // TODO: test more cases of different cache sizes.

    @Test
    public void testH5Freset_mdc_hit_rate_stats() {

        try {
            H5.H5Freset_mdc_hit_rate_stats(H5fid);
        }
        catch (Throwable err) {
            fail("H5.H5Freset_mdc_hit_rate_stats: " + err);
        }
    }

    @Test
    public void testH5Fget_name() {
        String fname = null;

        try {
            fname = H5.H5Fget_name(H5fid);
        }
        catch (Throwable err) {
            fail("H5.H5Fget_name: " + err);
        }
        assertNotNull(fname);
        assertEquals(fname, H5_FILE);
    }

    @Test
    public void testH5Fclear_elink_file_cache() {

        try {
            H5.H5Fclear_elink_file_cache(H5fid);
        }
        catch (Throwable err) {
            fail("H5.H5Freset_mdc_hit_rate_stats: " + err);
        }
    }

    @Test
    public void testH5F_dset_no_attrs_hint() {
        boolean ret_val_id = true;
        try {
            ret_val_id = H5.H5Fget_dset_no_attrs_hint(H5fid);
            assertFalse("H5F_dset_no_attrs_hint", ret_val_id);
            H5.H5Fset_dset_no_attrs_hint(H5fid, true);
            ret_val_id = H5.H5Fget_dset_no_attrs_hint(H5fid);
            assertTrue("H5F_dset_no_attrs_hint", ret_val_id);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5F_dset_no_attrs_hint: " + err);
        }
    }
}
